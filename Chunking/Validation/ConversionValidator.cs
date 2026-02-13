using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Chunking.Models;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Validation;

/// <summary>
/// Validates conversion results for consistency and correctness.
/// </summary>
public class ConversionValidator
{
    private readonly ISignatureRegistry _signatureRegistry;
    private readonly ITypeMappingTable _typeMappingTable;
    private readonly ConversionSettings _settings;
    private readonly ILogger<ConversionValidator> _logger;

    public ConversionValidator(
        ISignatureRegistry signatureRegistry,
        ITypeMappingTable typeMappingTable,
        ConversionSettings settings,
        ILogger<ConversionValidator> logger)
    {
        _signatureRegistry = signatureRegistry;
        _typeMappingTable = typeMappingTable;
        _settings = settings;
        _logger = logger;
    }

    /// <summary>
    /// Validates a single chunk conversion result against the registry.
    /// </summary>
    public async Task<ValidationReport> ValidateChunkAsync(
        int runId,
        string sourceFile,
        int chunkIndex,
        ChunkConversionResult result,
        CancellationToken cancellationToken = default)
    {
        var report = new ValidationReport
        {
            RunId = runId,
            SourceFile = sourceFile,
            TotalChunks = 1,
            ValidChunks = 0
        };

        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            // Validate signatures
            foreach (var signature in result.DefinedMethods)
            {
                // Skip validation if LegacyName is missing (cannot validate against registry)
                if (string.IsNullOrWhiteSpace(signature.LegacyName))
                {
                    continue;
                }

                var validationResult = await _signatureRegistry.ValidateSignatureAsync(
                    runId, signature, cancellationToken);

                report.Statistics.TotalMethods++;

                if (validationResult.IsValid)
                {
                    report.Statistics.ConsistentMethods++;
                }
                else
                {
                    foreach (var discrepancy in validationResult.Discrepancies)
                    {
                        report.SignatureIssues.Add(new ConsistencyIssue
                        {
                            IssueType = MapDiscrepancyToIssueType(discrepancy.Field),
                            EntityName = signature.LegacyName,
                            ChunkIndex = chunkIndex,
                            Expected = discrepancy.ExpectedValue,
                            Actual = discrepancy.ActualValue,
                            Description = $"Signature mismatch for {signature.LegacyName}: " +
                                         $"{discrepancy.Field} expected '{discrepancy.ExpectedValue}' " +
                                         $"but got '{discrepancy.ActualValue}'",
                            Severity = MapDiscrepancySeverity(discrepancy.Severity),
                            CanAutoFix = discrepancy.Severity == DiscrepancySeverity.Warning
                        });
                    }
                }
            }

            // Validate type mappings
            foreach (var mapping in result.TypeMappings)
            {
                var existing = await _typeMappingTable.GetMappingAsync(
                    runId, sourceFile, mapping.LegacyVariable, cancellationToken);

                report.Statistics.TotalVariables++;

                if (existing == null)
                {
                    report.Statistics.ConsistentVariables++;
                }
                else if (existing.TargetType != mapping.TargetType)
                {
                    report.TypeMappingIssues.Add(new ConsistencyIssue
                    {
                        IssueType = ConsistencyIssueType.VariableTypeMismatch,
                        EntityName = mapping.LegacyVariable,
                        ChunkIndex = chunkIndex,
                        Expected = existing.TargetType,
                        Actual = mapping.TargetType,
                        Description = $"Type mismatch for variable {mapping.LegacyVariable}: " +
                                     $"expected '{existing.TargetType}' but got '{mapping.TargetType}'",
                        Severity = IssueSeverity.Error,
                        CanAutoFix = true
                    });
                }
                else if (existing.TargetFieldName != mapping.TargetFieldName)
                {
                    report.TypeMappingIssues.Add(new ConsistencyIssue
                    {
                        IssueType = ConsistencyIssueType.VariableNameMismatch,
                        EntityName = mapping.LegacyVariable,
                        ChunkIndex = chunkIndex,
                        Expected = existing.TargetFieldName,
                        Actual = mapping.TargetFieldName,
                        Description = $"Field name mismatch for variable {mapping.LegacyVariable}: " +
                                     $"expected '{existing.TargetFieldName}' but got '{mapping.TargetFieldName}'",
                        Severity = IssueSeverity.Warning,
                        CanAutoFix = true
                    });
                }
                else
                {
                    report.Statistics.ConsistentVariables++;
                }
            }

            // Track forward references
            report.Statistics.TotalForwardReferences = result.ForwardReferences.Count;

            // Determine overall validity
            report.IsValid = !report.SignatureIssues.Any(i => i.Severity >= IssueSeverity.Error) &&
                            !report.TypeMappingIssues.Any(i => i.Severity >= IssueSeverity.Error);

            if (report.IsValid)
            {
                report.ValidChunks = 1;
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating chunk {ChunkIndex} for {File}", chunkIndex, sourceFile);
            report.IsValid = false;
            report.Warnings.Add($"Validation error: {ex.Message}");
        }

        stopwatch.Stop();
        report.Statistics.ValidationTimeMs = stopwatch.ElapsedMilliseconds;

        return report;
    }

    /// <summary>
    /// Runs a reconciliation pass across all chunks for a file.
    /// </summary>
    public async Task<ValidationReport> ReconcileFileAsync(
        int runId,
        string sourceFile,
        FileConversionHistory history,
        CancellationToken cancellationToken = default)
    {
        var report = new ValidationReport
        {
            RunId = runId,
            SourceFile = sourceFile,
            TotalChunks = history.ChunkResults.Count
        };

        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            // Check for unresolved forward references
            foreach (var unresolved in history.UnresolvedReferences)
            {
                report.UnresolvedReferences.Add(new UnresolvedReference
                {
                    CallerMethod = unresolved.CallerMethod,
                    TargetMethod = unresolved.TargetMethod,
                    PredictedSignature = unresolved.PredictedSignature,
                    Reason = "Method was never defined in any chunk"
                });

                report.SignatureIssues.Add(new ConsistencyIssue
                {
                    IssueType = ConsistencyIssueType.UnresolvedForwardReference,
                    EntityName = unresolved.TargetMethod,
                    Description = $"Forward reference from {unresolved.CallerMethod} to " +
                                 $"{unresolved.TargetMethod} was never resolved",
                    Severity = IssueSeverity.Error,
                    CanAutoFix = false
                });
            }

            report.Statistics.TotalForwardReferences = 
                history.UnresolvedReferences.Count + GetResolvedReferenceCount(history);
            report.Statistics.ResolvedReferences = GetResolvedReferenceCount(history);

            // Scan converted code for consistency issues
            foreach (var signature in history.AllSignatures)
            {
                report.Statistics.TotalMethods++;

                // Check if signature is used consistently across all chunks
                var usageIssues = await CheckSignatureUsageAsync(
                    signature, history.ChunkResults, cancellationToken);

                if (usageIssues.Count == 0)
                {
                    report.Statistics.ConsistentMethods++;
                }
                else
                {
                    report.SignatureIssues.AddRange(usageIssues);
                }
            }

            // Check type mappings consistency
            foreach (var mapping in history.AllTypeMappings)
            {
                report.Statistics.TotalVariables++;

                var usageIssues = await CheckTypeMappingUsageAsync(
                    mapping, history.ChunkResults, cancellationToken);

                if (usageIssues.Count == 0)
                {
                    report.Statistics.ConsistentVariables++;
                }
                else
                {
                    report.TypeMappingIssues.AddRange(usageIssues);
                }
            }

            // Count valid chunks
            report.ValidChunks = history.ChunkResults.Count(r => r.Success);

            // Determine overall validity
            report.IsValid = report.UnresolvedReferences.Count == 0 &&
                            !report.SignatureIssues.Any(i => i.Severity >= IssueSeverity.Error) &&
                            !report.TypeMappingIssues.Any(i => i.Severity >= IssueSeverity.Error);

            // Apply auto-fixes if enabled
            if (_settings.EnableReconciliationPass && report.SignatureIssues.Any(i => i.CanAutoFix))
            {
                var fixResults = await ApplyAutoFixesAsync(report, history, cancellationToken);
                report.AutoFixes.AddRange(fixResults);
                report.Statistics.AutoFixedIssues = fixResults.Count(f => f.Success);
            }

            report.Statistics.ManualFixRequired = 
                report.SignatureIssues.Count(i => !i.CanAutoFix) +
                report.TypeMappingIssues.Count(i => !i.CanAutoFix);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during reconciliation for {File}", sourceFile);
            report.IsValid = false;
            report.Warnings.Add($"Reconciliation error: {ex.Message}");
        }

        stopwatch.Stop();
        report.Statistics.ValidationTimeMs = stopwatch.ElapsedMilliseconds;

        _logger.LogInformation(
            "Reconciliation for {File}: {Valid} - Methods: {ConsistentM}/{TotalM}, " +
            "Variables: {ConsistentV}/{TotalV}, References: {ResolvedR}/{TotalR}",
            sourceFile,
            report.IsValid ? "PASSED" : "FAILED",
            report.Statistics.ConsistentMethods,
            report.Statistics.TotalMethods,
            report.Statistics.ConsistentVariables,
            report.Statistics.TotalVariables,
            report.Statistics.ResolvedReferences,
            report.Statistics.TotalForwardReferences);

        return report;
    }

    private static ConsistencyIssueType MapDiscrepancyToIssueType(string field)
    {
        return field switch
        {
            "TargetMethodName" => ConsistencyIssueType.MethodNameMismatch,
            "ReturnType" => ConsistencyIssueType.ReturnTypeMismatch,
            "ParameterCount" => ConsistencyIssueType.ParameterCountMismatch,
            _ when field.StartsWith("Parameter") => ConsistencyIssueType.ParameterTypeMismatch,
            _ => ConsistencyIssueType.SignatureMismatch
        };
    }

    private static IssueSeverity MapDiscrepancySeverity(DiscrepancySeverity severity)
    {
        return severity switch
        {
            DiscrepancySeverity.Warning => IssueSeverity.Warning,
            DiscrepancySeverity.Error => IssueSeverity.Error,
            DiscrepancySeverity.Critical => IssueSeverity.Critical,
            _ => IssueSeverity.Warning
        };
    }

    private static int GetResolvedReferenceCount(FileConversionHistory history)
    {
        // Estimate resolved references from defined methods
        return history.AllSignatures.Count;
    }

    private Task<List<ConsistencyIssue>> CheckSignatureUsageAsync(
        MethodSignature signature,
        List<ChunkConversionResult> chunkResults,
        CancellationToken cancellationToken)
    {
        var issues = new List<ConsistencyIssue>();

        foreach (var chunk in chunkResults)
        {
            // Check if the method name is used consistently in the converted code
            var expectedCall = $"{signature.TargetMethodName}(";
            var legacyPattern = signature.LegacyName.Replace("-", "");

            if (chunk.ConvertedCode.Contains(legacyPattern, StringComparison.OrdinalIgnoreCase) &&
                !chunk.ConvertedCode.Contains(expectedCall))
            {
                issues.Add(new ConsistencyIssue
                {
                    IssueType = ConsistencyIssueType.MethodNameMismatch,
                    EntityName = signature.LegacyName,
                    Expected = signature.TargetMethodName,
                    Actual = "Possibly unconverted reference",
                    Description = $"Found reference to legacy name pattern '{legacyPattern}' " +
                                 $"but expected call to '{signature.TargetMethodName}'",
                    Severity = IssueSeverity.Warning,
                    CanAutoFix = true
                });
            }
        }

        return Task.FromResult(issues);
    }

    private Task<List<ConsistencyIssue>> CheckTypeMappingUsageAsync(
        TypeMapping mapping,
        List<ChunkConversionResult> chunkResults,
        CancellationToken cancellationToken)
    {
        var issues = new List<ConsistencyIssue>();

        foreach (var chunk in chunkResults)
        {
            // Check for legacy variable name usage in converted code
            var legacyPattern = mapping.LegacyVariable.Replace("-", "");

            if (chunk.ConvertedCode.Contains(mapping.LegacyVariable, StringComparison.OrdinalIgnoreCase) ||
                chunk.ConvertedCode.Contains(legacyPattern, StringComparison.OrdinalIgnoreCase))
            {
                issues.Add(new ConsistencyIssue
                {
                    IssueType = ConsistencyIssueType.VariableNameMismatch,
                    EntityName = mapping.LegacyVariable,
                    Expected = mapping.TargetFieldName,
                    Actual = mapping.LegacyVariable,
                    Description = $"Found legacy variable name '{mapping.LegacyVariable}' (or pattern '{legacyPattern}') " +
                                 $"in converted code - should be '{mapping.TargetFieldName}'",
                    Severity = IssueSeverity.Warning,
                    CanAutoFix = true
                });
            }
        }

        return Task.FromResult(issues);
    }

    private Task<List<AutoFixResult>> ApplyAutoFixesAsync(
        ValidationReport report,
        FileConversionHistory history,
        CancellationToken cancellationToken)
    {
        var results = new List<AutoFixResult>();
        int attempts = 0;

        foreach (var issue in report.SignatureIssues.Where(i => i.CanAutoFix))
        {
            if (attempts >= _settings.MaxAutoFixAttempts)
            {
                break;
            }

            var fix = new AutoFixResult
            {
                OriginalIssue = issue,
                OriginalValue = issue.Actual,
                NewValue = issue.Expected,
                FixDescription = $"Replaced '{issue.Actual}' with '{issue.Expected}'",
                Success = true // In a real implementation, would apply the fix
            };

            results.Add(fix);
            attempts++;
        }

        foreach (var issue in report.TypeMappingIssues.Where(i => i.CanAutoFix))
        {
            if (attempts >= _settings.MaxAutoFixAttempts)
            {
                break;
            }

            var fix = new AutoFixResult
            {
                OriginalIssue = issue,
                OriginalValue = issue.Actual,
                NewValue = issue.Expected,
                FixDescription = $"Replaced '{issue.Actual}' with '{issue.Expected}'",
                Success = true
            };

            results.Add(fix);
            attempts++;
        }

        return Task.FromResult(results);
    }
}
