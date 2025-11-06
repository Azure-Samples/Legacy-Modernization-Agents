using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CustomerMasterUpdate
{
    /// <summary>
    /// Main program for updating and indexing customer master based on transaction history.
    /// Reads sequential transaction input, updates an indexed master file, and produces a report.
    /// </summary>
    public class CustomerMasterUpdater
    {
        private readonly string _transactionFilePath;
        private readonly string _customerMasterFilePath;
        private readonly string _reportFilePath;

        private readonly ICustomerMasterRepository _customerMasterRepository;
        private readonly ITransactionReader _transactionReader;
        private readonly IReportWriter _reportWriter;

        // Segment bonus multipliers (segment code -> multiplier)
        private readonly List<SegmentBonus> _segmentBonuses = new()
        {
            new SegmentBonus("S", 1.0m),
            new SegmentBonus("V", 2.0m),
            new SegmentBonus("X", 1.5m)
        };

        // Counters
        private int _readCount = 0;
        private int _writeCount = 0;
        private int _updateCount = 0;
        private int _skipCount = 0;

        /// <summary>
        /// Initializes a new instance of the <see cref="CustomerMasterUpdater"/> class.
        /// </summary>
        /// <param name="transactionFilePath">Path to the transaction input file.</param>
        /// <param name="customerMasterFilePath">Path to the customer master file.</param>
        /// <param name="reportFilePath">Path to the report output file.</param>
        /// <param name="customerMasterRepository">Repository for customer master records.</param>
        /// <param name="transactionReader">Reader for transaction records.</param>
        /// <param name="reportWriter">Writer for report output.</param>
        public CustomerMasterUpdater(
            string transactionFilePath,
            string customerMasterFilePath,
            string reportFilePath,
            ICustomerMasterRepository? customerMasterRepository = null,
            ITransactionReader? transactionReader = null,
            IReportWriter? reportWriter = null)
        {
            _transactionFilePath = transactionFilePath;
            _customerMasterFilePath = customerMasterFilePath;
            _reportFilePath = reportFilePath;

            _customerMasterRepository = customerMasterRepository ?? new FileCustomerMasterRepository(_customerMasterFilePath);
            _transactionReader = transactionReader ?? new FileTransactionReader(_transactionFilePath);
            _reportWriter = reportWriter ?? new FileReportWriter(_reportFilePath);
        }

        /// <summary>
        /// Runs the customer master update process asynchronously.
        /// </summary>
        public async Task RunAsync()
        {
            try
            {
                await InitializeAsync();
                await OpenFilesAsync();

                await _reportWriter.WriteLineAsync(Header1);
                await _reportWriter.WriteLineAsync(Header2);

                await foreach (var transaction in _transactionReader.ReadTransactionsAsync())
                {
                    _readCount++;
                    await ProcessTransactionAsync(transaction);
                }

                await WriteReportAsync();
            }
            catch (Exception ex)
            {
                await HandleFileErrorAsync("FILE I/O ERROR", ex);
            }
            finally
            {
                await CloseFilesAsync();
            }
        }

        /// <summary>
        /// Initializes counters and segment bonus table.
        /// </summary>
        private Task InitializeAsync()
        {
            _readCount = 0;
            _writeCount = 0;
            _updateCount = 0;
            _skipCount = 0;
            // Segment bonus table is already initialized in field initializer.
            return Task.CompletedTask;
        }

        /// <summary>
        /// Opens all files required for processing.
        /// </summary>
        private async Task OpenFilesAsync()
        {
            try
            {
                await _customerMasterRepository.OpenAsync();
                await _transactionReader.OpenAsync();
                await _reportWriter.OpenAsync();
            }
            catch (Exception ex)
            {
                await HandleFileErrorAsync("RPT OPEN FAILED", ex);
                throw;
            }
        }

        /// <summary>
        /// Closes all files.
        /// </summary>
        private async Task CloseFilesAsync()
        {
            await _transactionReader.CloseAsync();
            await _customerMasterRepository.CloseAsync();
            await _reportWriter.CloseAsync();
        }

        /// <summary>
        /// Processes a single transaction record.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private async Task ProcessTransactionAsync(TransactionRecord transaction)
        {
            NormalizeName(transaction);

            switch (transaction.Type)
            {
                case TransactionType.New:
                    await CreateCustomerAsync(transaction);
                    break;
                case TransactionType.Update:
                    await UpdateCustomerAsync(transaction);
                    break;
                case TransactionType.Purchase:
                    await ApplyPurchaseAsync(transaction);
                    break;
                case TransactionType.Return:
                    await ApplyReturnAsync(transaction);
                    break;
                default:
                    _skipCount++;
                    break;
            }
        }

        /// <summary>
        /// Creates a new customer record, or updates if already exists.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private async Task CreateCustomerAsync(TransactionRecord transaction)
        {
            var customer = new CustomerMasterRecord
            {
                CustomerId = transaction.CustomerId,
                Name = transaction.Name,
                JoinDate = transaction.Date,
                Segment = "S",
                Balance = 0m,
                Points = 0,
                LastOrderDate = 0
            };

            try
            {
                await _customerMasterRepository.AddAsync(customer);
                _writeCount++;
            }
            catch (DuplicateCustomerException)
            {
                // If already exists, update instead
                await UpdateCustomerAsync(transaction);
            }
        }

        /// <summary>
        /// Updates an existing customer record.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private async Task UpdateCustomerAsync(TransactionRecord transaction)
        {
            var customer = await _customerMasterRepository.GetByIdAsync(transaction.CustomerId);
            if (customer == null)
            {
                _skipCount++;
                return;
            }

            if (!string.IsNullOrWhiteSpace(transaction.Name) && !string.Equals(transaction.Name, customer.Name, StringComparison.Ordinal))
            {
                customer.Name = transaction.Name;
            }

            if (transaction.Amount > 100_000m)
            {
                customer.Segment = "V";
            }

            try
            {
                await _customerMasterRepository.UpdateAsync(customer);
                _updateCount++;
            }
            catch (Exception)
            {
                _skipCount++;
            }
        }

        /// <summary>
        /// Applies a purchase transaction to a customer.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private async Task ApplyPurchaseAsync(TransactionRecord transaction)
        {
            var customer = await _customerMasterRepository.GetByIdAsync(transaction.CustomerId);
            if (customer == null)
            {
                _skipCount++;
                return;
            }

            customer.Balance += transaction.Amount;
            customer.LastOrderDate = transaction.Date;

            var pointsToAdd = CalculatePoints(customer.Segment, transaction.Amount);
            customer.Points += pointsToAdd;

            await _customerMasterRepository.UpdateAsync(customer);
            _updateCount++;
        }

        /// <summary>
        /// Applies a return transaction to a customer.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private async Task ApplyReturnAsync(TransactionRecord transaction)
        {
            var customer = await _customerMasterRepository.GetByIdAsync(transaction.CustomerId);
            if (customer == null)
            {
                _skipCount++;
                return;
            }

            customer.Balance -= transaction.Amount;
            var pointsToSubtract = (int)(transaction.Amount / 100m);
            customer.Points -= pointsToSubtract;

            await _customerMasterRepository.UpdateAsync(customer);
            _updateCount++;
        }

        /// <summary>
        /// Calculates points to add for a purchase, using segment multiplier.
        /// </summary>
        /// <param name="segment">Customer segment code.</param>
        /// <param name="amount">Purchase amount.</param>
        /// <returns>Points to add.</returns>
        private int CalculatePoints(string segment, decimal amount)
        {
            var basePoints = (int)(amount / 100m);
            var bonus = _segmentBonuses.FirstOrDefault(b => b.Segment == segment);
            if (bonus != null)
            {
                return (int)Math.Round(basePoints * bonus.Multiplier, MidpointRounding.AwayFromZero);
            }
            return basePoints;
        }

        /// <summary>
        /// Normalizes the name field: trims and collapses spaces.
        /// </summary>
        /// <param name="transaction">The transaction record.</param>
        private static void NormalizeName(TransactionRecord transaction)
        {
            if (string.IsNullOrWhiteSpace(transaction.Name))
            {
                transaction.Name = string.Empty;
                return;
            }

            // Collapse multiple spaces and trim
            transaction.Name = string.Join(' ', transaction.Name
                .Split(' ', StringSplitOptions.RemoveEmptyEntries));
        }

        /// <summary>
        /// Writes the report file: totals and customer listing.
        /// </summary>
        private async Task WriteReportAsync()
        {
            var totalsLine = $"Totals - Read: {_readCount}  Updated: {_updateCount}  Written: {_writeCount}  Skipped: {_skipCount}";
            await _reportWriter.WriteLineAsync(totalsLine);

            await foreach (var customer in _customerMasterRepository.GetAllAsync())
            {
                var line = FormatCustomerLine(customer);
                await _reportWriter.WriteLineAsync(line);
            }
        }

        /// <summary>
        /// Formats a customer record for report output.
        /// </summary>
        /// <param name="customer">The customer record.</param>
        /// <returns>Formatted string for report.</returns>
        private static string FormatCustomerLine(CustomerMasterRecord customer)
        {
            var name = customer.Name?.Trim() ?? string.Empty;
            var paddedName = name.PadRight(30);
            var balanceStr = customer.Balance.ToString("F2", CultureInfo.InvariantCulture).PadLeft(10);
            var pointsStr = customer.Points.ToString().PadLeft(7);
            var lastOrderStr = customer.LastOrderDate.ToString("D8");

            return $"{customer.CustomerId,-10} | {paddedName} | {customer.Segment} | {balanceStr} | {pointsStr} | {lastOrderStr}";
        }

        /// <summary>
        /// Handles file I/O errors by logging and displaying error information.
        /// </summary>
        /// <param name="message">Error message.</param>
        /// <param name="ex">Exception thrown.</param>
        private static Task HandleFileErrorAsync(string message, Exception ex)
        {
            Console.Error.WriteLine($"MEDIUM-MOD-TEST - {message}: {ex.Message}");
            // In production, log exception details here.
            return Task.CompletedTask;
        }

        private const string Header1 = "Customer Activity Report";
        private const string Header2 = "ID        | Name                          | Seg | Balance   | Points  | LastOrder";
    }

    #region Data Models

    /// <summary>
    /// Represents a transaction record from the input file.
    /// </summary>
    public record TransactionRecord
    {
        /// <summary>
        /// Customer ID (10 chars).
        /// </summary>
        public string CustomerId { get; init; } = string.Empty;

        /// <summary>
        /// Transaction type: N=New, U=Update, P=Purchase, R=Return.
        /// </summary>
        public TransactionType Type { get; init; }

        /// <summary>
        /// Transaction amount (decimal, 2 decimals).
        /// </summary>
        public decimal Amount { get; init; }

        /// <summary>
        /// Transaction date (YYYYMMDD as int).
        /// </summary>
        public int Date { get; init; }

        /// <summary>
        /// Customer name (30 chars).
        /// </summary>
        public string Name { get; set; } = string.Empty;
    }

    /// <summary>
    /// Transaction type enumeration.
    /// </summary>
    public enum TransactionType
    {
        Unknown,
        New,
        Update,
        Purchase,
        Return
    }

    /// <summary>
    /// Represents a customer master record.
    /// </summary>
    public record CustomerMasterRecord
    {
        /// <summary>
        /// Customer ID (10 chars).
        /// </summary>
        public string CustomerId { get; set; } = string.Empty;

        /// <summary>
        /// Customer name (30 chars).
        /// </summary>
        public string Name { get; set; } = string.Empty;

        /// <summary>
        /// Join date (YYYYMMDD as int).
        /// </summary>
        public int JoinDate { get; set; }

        /// <summary>
        /// Segment code: S=Standard, V=VIP, X=Other.
        /// </summary>
        public string Segment { get; set; } = "S";

        /// <summary>
        /// Account balance (decimal, 2 decimals).
        /// </summary>
        public decimal Balance { get; set; }

        /// <summary>
        /// Loyalty points (integer).
        /// </summary>
        public int Points { get; set; }

        /// <summary>
        /// Last order date (YYYYMMDD as int).
        /// </summary>
        public int LastOrderDate { get; set; }
    }

    /// <summary>
    /// Represents a segment bonus multiplier.
    /// </summary>
    public record SegmentBonus(string Segment, decimal Multiplier);

    #endregion

    #region Repository and File Interfaces

    /// <summary>
    /// Interface for reading transaction records.
    /// </summary>
    public interface ITransactionReader : IAsyncDisposable
    {
        /// <summary>
        /// Opens the transaction file.
        /// </summary>
        Task OpenAsync();

        /// <summary>
        /// Reads all transaction records asynchronously.
        /// </summary>
        /// <returns>Async enumerable of transaction records.</returns>
        IAsyncEnumerable<TransactionRecord> ReadTransactionsAsync();

        /// <summary>
        /// Closes the transaction file.
        /// </summary>
        Task CloseAsync();
    }

    /// <summary>
    /// Interface for writing report output.
    /// </summary>
    public interface IReportWriter : IAsyncDisposable
    {
        /// <summary>
        /// Opens the report file.
        /// </summary>
        Task OpenAsync();

        /// <summary>
        /// Writes a line to the report file.
        /// </summary>
        /// <param name="line">The line to write.</param>
        Task WriteLineAsync(string line);

        /// <summary>
        /// Closes the report file.
        /// </summary>
        Task CloseAsync();
    }

    /// <summary>
    /// Interface for customer master repository.
    /// </summary>
    public interface ICustomerMasterRepository : IAsyncDisposable
    {
        /// <summary>
        /// Opens the customer master file.
        /// </summary>
        Task OpenAsync();

        /// <summary>
        /// Gets a customer by ID.
        /// </summary>
        /// <param name="customerId">Customer ID.</param>
        /// <returns>Customer record or null if not found.</returns>
        Task<CustomerMasterRecord?> GetByIdAsync(string customerId);

        /// <summary>
        /// Adds a new customer.
        /// </summary>
        /// <param name="customer">Customer record.</param>
        /// <exception cref="DuplicateCustomerException">If customer already exists.</exception>
        Task AddAsync(CustomerMasterRecord customer);

        /// <summary>
        /// Updates an existing customer.
        /// </summary>
        /// <param name="customer">Customer record.</param>
        Task UpdateAsync(CustomerMasterRecord customer);

        /// <summary>
        /// Gets all customers in the master file.
        /// </summary>
        /// <returns>Async enumerable of customer records.</returns>
        IAsyncEnumerable<CustomerMasterRecord> GetAllAsync();

        /// <summary>
        /// Closes the customer master file.
        /// </summary>
        Task CloseAsync();
    }

    /// <summary>
    /// Exception thrown when attempting to add a duplicate customer.
    /// </summary>
    public class DuplicateCustomerException : Exception
    {
        public DuplicateCustomerException(string customerId)
            : base($"Customer with ID '{customerId}' already exists.") { }
    }

    #endregion

    #region File Implementations

    /// <summary>
    /// Reads transaction records from a fixed-width file.
    /// </summary>
    public class FileTransactionReader : ITransactionReader
    {
        private readonly string _filePath;
        private StreamReader? _reader;

        public FileTransactionReader(string filePath)
        {
            _filePath = filePath;
        }

        public Task OpenAsync()
        {
            _reader = new StreamReader(_filePath, Encoding.UTF8);
            return Task.CompletedTask;
        }

        public async IAsyncEnumerable<TransactionRecord> ReadTransactionsAsync()
        {
            if (_reader == null)
                throw new InvalidOperationException("Transaction file not open.");

            string? line;
            while ((line = await _reader.ReadLineAsync()) != null)
            {
                if (line.Length < 80)
                    continue; // Skip invalid lines

                yield return ParseTransaction(line);
            }
        }

        public Task CloseAsync()
        {
            _reader?.Dispose();
            _reader = null;
            return Task.CompletedTask;
        }

        public ValueTask DisposeAsync()
        {
            _reader?.Dispose();
            return ValueTask.CompletedTask;
        }

        private static TransactionRecord ParseTransaction(string line)
        {
            // Field positions based on COBOL FD
            var customerId = line.Substring(0, 10).Trim();
            var typeChar = line.Substring(10, 1);
            var amountRaw = line.Substring(11, 9);
            var dateStr = line.Substring(20, 8);
            var name = line.Substring(28, 30).Trim();

            var type = typeChar switch
            {
                "N" => TransactionType.New,
                "U" => TransactionType.Update,
                "P" => TransactionType.Purchase,
                "R" => TransactionType.Return,
                _ => TransactionType.Unknown
            };

            decimal amount = 0;
            if (decimal.TryParse(amountRaw.Insert(amountRaw.Length - 2, "."), NumberStyles.Number, CultureInfo.InvariantCulture, out var amt))
                amount = amt;

            int date = 0;
            int.TryParse(dateStr, out date);

            return new TransactionRecord
            {
                CustomerId = customerId,
                Type = type,
                Amount = amount,
                Date = date,
                Name = name
            };
        }
    }

    /// <summary>
    /// Writes report output to a text file.
    /// </summary>
    public class FileReportWriter : IReportWriter
    {
        private readonly string _filePath;
        private StreamWriter? _writer;

        public FileReportWriter(string filePath)
        {
            _filePath = filePath;
        }

        public Task OpenAsync()
        {
            _writer = new StreamWriter(_filePath, false, Encoding.UTF8);
            return Task.CompletedTask;
        }

        public async Task WriteLineAsync(string line)
        {
            if (_writer == null)
                throw new InvalidOperationException("Report file not open.");

            await _writer.WriteLineAsync(line);
        }

        public Task CloseAsync()
        {
            _writer?.Dispose();
            _writer = null;
            return Task.CompletedTask;
        }

        public ValueTask DisposeAsync()
        {
            _writer?.Dispose();
            return ValueTask.CompletedTask;
        }
    }

    /// <summary>
    /// Implements a simple indexed customer master repository using a file.
    /// </summary>
    public class FileCustomerMasterRepository : ICustomerMasterRepository
    {
        private readonly string _filePath;
        private Dictionary<string, CustomerMasterRecord>? _customers;

        public FileCustomerMasterRepository(string filePath)
        {
            _filePath = filePath;
        }

        public Task OpenAsync()
        {
            _customers = new Dictionary<string, CustomerMasterRecord>();

            if (File.Exists(_filePath))
            {
                foreach (var line in File.ReadLines(_filePath))
                {
                    var record = ParseCustomer(line);
                    if (!string.IsNullOrWhiteSpace(record.CustomerId))
                        _customers[record.CustomerId] = record;
                }
            }

            return Task.CompletedTask;
        }

        public Task<CustomerMasterRecord?> GetByIdAsync(string customerId)
        {
            if (_customers == null)
                throw new InvalidOperationException("Customer master file not open.");

            _customers.TryGetValue(customerId, out var record);
            return Task.FromResult(record);
        }

        public Task AddAsync(CustomerMasterRecord customer)
        {
            if (_customers == null)
                throw new InvalidOperationException("Customer master file not open.");

            if (_customers.ContainsKey(customer.CustomerId))
                throw new DuplicateCustomerException(customer.CustomerId);

            _customers[customer.CustomerId] = customer;
            return Task.CompletedTask;
        }

        public Task UpdateAsync(CustomerMasterRecord customer)
        {
            if (_customers == null)
                throw new InvalidOperationException("Customer master file not open.");

            _customers[customer.CustomerId] = customer;
            return Task.CompletedTask;
        }

        public async IAsyncEnumerable<CustomerMasterRecord> GetAllAsync()
        {
            if (_customers == null)
                throw new InvalidOperationException("Customer master file not open.");

            foreach (var record in _customers.Values.OrderBy(c => c.CustomerId))
            {
                yield return record;
                await Task.Yield();
            }
        }

        public Task CloseAsync()
        {
            if (_customers != null)
            {
                using var writer = new StreamWriter(_filePath, false, Encoding.UTF8);
                foreach (var record in _customers.Values)
                {
                    writer.WriteLine(FormatCustomer(record));
                }
                _customers = null;
            }
            return Task.CompletedTask;
        }

        public ValueTask DisposeAsync()
        {
            _customers = null;
            return ValueTask.CompletedTask;
        }

        private static CustomerMasterRecord ParseCustomer(string line)
        {
            // Field positions based on COBOL FD
            if (line.Length < 68)
                return new CustomerMasterRecord();

            var customerId = line.Substring(0, 10).Trim();
            var name = line.Substring(10, 30).Trim();
            var joinDateStr = line.Substring(40, 8);
            var segment = line.Substring(48, 1);
            var balanceStr = line.Substring(49, 9);
            var pointsStr = line.Substring(58, 7);
            var lastOrderDateStr = line.Substring(65, 8);

            int joinDate = 0, lastOrderDate = 0;
            int.TryParse(joinDateStr, out joinDate);
            int.TryParse(lastOrderDateStr, out lastOrderDate);

            decimal balance = 0;
            if (decimal.TryParse(balanceStr.Insert(balanceStr.Length - 2, "."), NumberStyles.Number, CultureInfo.InvariantCulture, out var bal))
                balance = bal;

            int points = 0;
            int.TryParse(pointsStr, out points);

            return new CustomerMasterRecord
            {
                CustomerId = customerId,
                Name = name,
                JoinDate = joinDate,
                Segment = segment,
                Balance = balance,
                Points = points,
                LastOrderDate = lastOrderDate
            };
        }

        private static string FormatCustomer(CustomerMasterRecord record)
        {
            // Fixed-width format for master file
            var customerId = record.CustomerId.PadRight(10);
            var name = (record.Name ?? string.Empty).PadRight(30);
            var joinDate = record.JoinDate.ToString("D8");
            var segment = (record.Segment ?? "S").PadRight(1);
            var balance = ((int)(record.Balance * 100)).ToString().PadLeft(9, '0');
            var points = record.Points.ToString().PadLeft(7, '0');
            var lastOrderDate = record.LastOrderDate.ToString("D8");

            return $"{customerId}{name}{joinDate}{segment}{balance}{points}{lastOrderDate}";
        }
    }

    #endregion

    #region Program Entry Point

    /// <summary>
    /// Program entry point.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Main entry point.
        /// </summary>
        public static async Task Main(string[] args)
        {
            var transactionFile = "TRANS.DAT";
            var customerMasterFile = "CUST.MST";
            var reportFile = "REPORT.TXT";

            var updater = new CustomerMasterUpdater(transactionFile, customerMasterFile, reportFile);
            await updater.RunAsync();
        }
    }

    #endregion
}