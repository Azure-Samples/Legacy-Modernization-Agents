using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CobolModernization.Agents.Interfaces;
using CobolModernization.Helpers;
using CobolModernization.Models;
using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;

namespace CobolModernization.Agents
{
    /// <summary>
    /// AI-powered validation agent that compares converted code with original COBOL
    /// and generates detailed validation reports with accuracy scoring
    /// </summary>
    public class ValidationAgent : IValidationAgent
    {
        private readonly Kernel _kernel;
        private readonly IChatCompletionService _chatCompletion;
        private readonly ILogger<ValidationAgent> _logger;
        private readonly ChatLogger _chatLogger;
        private readonly EnhancedLogger? _enhancedLogger;

        public ValidationAgent(
            Kernel kernel,
            ILogger<ValidationAgent> logger,
            ChatLogger chatLogger,
            EnhancedLogger? enhancedLogger = null)
        {
            _kernel = kernel ?? throw new ArgumentNullException(nameof(kernel));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
            _chatLogger = chatLogger ?? throw new ArgumentNullException(nameof(chatLogger));
            _enhancedLogger = enhancedLogger;
            _chatCompletion = _kernel.GetRequiredService<IChatCompletionService>();
        }

        /// <inheritdoc/>
        public async Task<ValidationReport> ValidateConversionAsync(
            string cobolSourcePath,
            string convertedCodePath,
            string targetLanguage,
            string outputPath)
        {
            _logger.LogInformation("Starting validation of {Language} conversion", targetLanguage);
            _enhancedLogger?.LogBehindTheScenes("VALIDATION", "VALIDATION_START",
                $"Starting AI-powered validation of {targetLanguage} conversion");

            var report = new ValidationReport
            {
                TargetLanguage = targetLanguage,
                Status = ValidationStatus.ValidationFailed,
                ValidationTimestamp = DateTime.UtcNow,
                ReportPath = outputPath
            };

            try
            {
                // Gather COBOL source files
                var cobolFiles = GatherCobolFiles(cobolSourcePath);
                report.CobolFilesAnalyzed = cobolFiles.Count;

                if (cobolFiles.Count == 0)
                {
                    _logger.LogWarning("No COBOL files found in {Path}", cobolSourcePath);
                    _enhancedLogger?.LogBehindTheScenes("VALIDATION", "NO_COBOL_FILES",
                        $"No COBOL files found in {cobolSourcePath}");
                    report.DetailedAnalysis = "No COBOL source files found for validation.";
                    await GenerateMarkdownReport(report);
                    return report;
                }

                // Gather converted files
                var convertedFiles = GatherConvertedFiles(convertedCodePath, targetLanguage);
                report.ConvertedFilesAnalyzed = convertedFiles.Count;

                if (convertedFiles.Count == 0)
                {
                    _logger.LogWarning("No converted files found in {Path}", convertedCodePath);
                    _enhancedLogger?.LogBehindTheScenes("VALIDATION", "NO_CONVERTED_FILES",
                        $"No {targetLanguage} files found in {convertedCodePath}");
                    report.DetailedAnalysis = "No converted files found for validation.";
                    await GenerateMarkdownReport(report);
                    return report;
                }

                // Read all source files
                _logger.LogInformation("Reading {CobolCount} COBOL files and {ConvertedCount} {Language} files",
                    cobolFiles.Count, convertedFiles.Count, targetLanguage);
                _enhancedLogger?.LogBehindTheScenes("VALIDATION", "FILE_READING",
                    $"Reading {cobolFiles.Count} COBOL files and {convertedFiles.Count} {targetLanguage} files for comparison");

                var cobolContent = await ReadAllFilesAsync(cobolFiles);
                var convertedContent = await ReadAllFilesAsync(convertedFiles);

                // Perform AI-powered validation
                _logger.LogInformation("Requesting AI validation analysis...");
                _enhancedLogger?.LogBehindTheScenes("VALIDATION", "AI_ANALYSIS_START",
                    $"Sending validation request to AI model for {targetLanguage} conversion analysis");

                var analysisResult = await PerformAIValidation(cobolContent, convertedContent, targetLanguage);

                // Parse analysis result
                ParseAnalysisResult(analysisResult, report);

                // Generate markdown report
                await GenerateMarkdownReport(report);

                _logger.LogInformation("Validation completed. Accuracy: {Accuracy}%, Status: {Status}",
                    report.AccuracyScore, report.Status);
                _enhancedLogger?.LogBehindTheScenes("VALIDATION", "VALIDATION_COMPLETE",
                    $"Validation completed: {report.AccuracyScore:F1}% accuracy, {report.Status}, {report.Differences.Count} differences found");

                return report;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Validation failed with error");
                _enhancedLogger?.LogBehindTheScenes("VALIDATION", "VALIDATION_ERROR",
                    $"Validation failed: {ex.Message}");
                report.Status = ValidationStatus.ValidationFailed;
                report.DetailedAnalysis = $"Validation failed: {ex.Message}";
                await GenerateMarkdownReport(report);
                return report;
            }
        }

        private List<string> GatherCobolFiles(string path)
        {
            var files = new List<string>();

            if (!Directory.Exists(path))
            {
                _logger.LogWarning("COBOL source path does not exist: {Path}", path);
                return files;
            }

            // Look for .cbl and .cpy files
            files.AddRange(Directory.GetFiles(path, "*.cbl", SearchOption.AllDirectories));
            files.AddRange(Directory.GetFiles(path, "*.cpy", SearchOption.AllDirectories));
            files.AddRange(Directory.GetFiles(path, "*.CBL", SearchOption.AllDirectories));
            files.AddRange(Directory.GetFiles(path, "*.CPY", SearchOption.AllDirectories));

            return files.Distinct().OrderBy(f => f).ToList();
        }

        private List<string> GatherConvertedFiles(string path, string targetLanguage)
        {
            var files = new List<string>();

            if (!Directory.Exists(path))
            {
                _logger.LogWarning("Converted code path does not exist: {Path}", path);
                return files;
            }

            string extension = targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ? "*.cs" : "*.java";
            files.AddRange(Directory.GetFiles(path, extension, SearchOption.AllDirectories));

            return files.Distinct().OrderBy(f => f).ToList();
        }

        private async Task<Dictionary<string, string>> ReadAllFilesAsync(List<string> filePaths)
        {
            var content = new Dictionary<string, string>();

            foreach (var filePath in filePaths)
            {
                try
                {
                    var fileName = Path.GetFileName(filePath);
                    var fileContent = await File.ReadAllTextAsync(filePath);
                    content[fileName] = fileContent;
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "Failed to read file: {FilePath}", filePath);
                }
            }

            return content;
        }

        private async Task<string> PerformAIValidation(
            Dictionary<string, string> cobolContent,
            Dictionary<string, string> convertedContent,
            string targetLanguage)
        {
            var prompt = BuildValidationPrompt(cobolContent, convertedContent, targetLanguage);

            var chatHistory = new ChatHistory();
            chatHistory.AddSystemMessage(@"You are an expert code validation specialist with deep knowledge of:
- COBOL programming and mainframe systems
- Modern programming languages (C#, Java)
- Code migration and modernization patterns
- Functional equivalence analysis

Your task is to compare converted code with original COBOL and provide:
1. Detailed functional analysis
2. Accuracy score (0-100)
3. List of differences categorized by severity
4. Specific recommendations for fixes

Be thorough, specific, and actionable in your analysis.");

            chatHistory.AddUserMessage(prompt);

            // Log the conversation
            _chatLogger.LogUserMessage("ValidationAgent", targetLanguage, prompt);

            // Track API call with EnhancedLogger
            var stopwatch = System.Diagnostics.Stopwatch.StartNew();
            int apiCallId = 0;

            if (_enhancedLogger != null)
            {
                apiCallId = _enhancedLogger.LogApiCallStart(
                    "ValidationAgent",
                    "ChatCompletion",
                    "OpenAI/ValidateConversion",
                    "gpt-4",
                    $"Validating {targetLanguage} conversion ({cobolContent.Count} COBOL files, {convertedContent.Count} {targetLanguage} files)"
                );
            }

            try
            {
                var response = await _chatCompletion.GetChatMessageContentAsync(
                    chatHistory,
                    new Microsoft.SemanticKernel.PromptExecutionSettings
                    {
                        ExtensionData = new Dictionary<string, object>
                        {
                            ["max_tokens"] = 16000,
                            ["temperature"] = 0.2 // Low temperature for consistent analysis
                        }
                    });

                stopwatch.Stop();

                var result = response.Content ?? "No analysis generated";

                // Log the response
                _chatLogger.LogAIResponse("ValidationAgent", targetLanguage, result);

                // Track API call success
                if (_enhancedLogger != null && apiCallId > 0)
                {
                    // Estimate tokens based on response length (rough estimate: 1 token ≈ 4 characters)
                    int tokensUsed = result.Length / 4;

                    // Estimate cost based on tokens (rough estimate for GPT-4)
                    decimal cost = tokensUsed * 0.00003m; // $0.03 per 1K tokens

                    _enhancedLogger.LogApiCallEnd(apiCallId, result, tokensUsed, cost);
                }

                return result;
            }
            catch (Exception ex)
            {
                stopwatch.Stop();

                // Track API call failure
                if (_enhancedLogger != null && apiCallId > 0)
                {
                    _enhancedLogger.LogApiCallError(apiCallId, ex.Message);
                }

                _logger.LogError(ex, "Failed to perform AI validation");
                throw;
            }
        }

        private string BuildValidationPrompt(
            Dictionary<string, string> cobolContent,
            Dictionary<string, string> convertedContent,
            string targetLanguage)
        {
            var sb = new StringBuilder();

            sb.AppendLine("# COBOL to " + targetLanguage + " Conversion Validation");
            sb.AppendLine();
            sb.AppendLine("## Task");
            sb.AppendLine("Compare the original COBOL code with the converted " + targetLanguage + " code and validate functional equivalence.");
            sb.AppendLine();

            // Calculate total content size
            int totalCobolSize = cobolContent.Sum(f => f.Value.Length);
            int totalConvertedSize = convertedContent.Sum(f => f.Value.Length);
            int maxCharsPerFile = 3000; // Reduced from 10000 to prevent timeouts

            // Add COBOL source with aggressive truncation
            sb.AppendLine("## Original COBOL Code");
            sb.AppendLine();
            sb.AppendLine($"Total COBOL Files: {cobolContent.Count}, Total Size: {totalCobolSize:N0} characters");
            sb.AppendLine();

            foreach (var file in cobolContent.Take(10)) // Limit to 10 files max
            {
                sb.AppendLine($"### File: {file.Key}");
                sb.AppendLine("```cobol");
                if (file.Value.Length > maxCharsPerFile)
                {
                    sb.AppendLine(file.Value.Substring(0, maxCharsPerFile));
                    sb.AppendLine($"... (truncated, {file.Value.Length - maxCharsPerFile:N0} chars omitted)");
                }
                else
                {
                    sb.AppendLine(file.Value);
                }
                sb.AppendLine("```");
                sb.AppendLine();
            }

            if (cobolContent.Count > 10)
            {
                sb.AppendLine($"... and {cobolContent.Count - 10} more COBOL files not shown");
                sb.AppendLine();
            }

            // Add converted code with aggressive truncation
            sb.AppendLine($"## Converted {targetLanguage} Code");
            sb.AppendLine();
            sb.AppendLine($"Total {targetLanguage} Files: {convertedContent.Count}, Total Size: {totalConvertedSize:N0} characters");
            sb.AppendLine();

            foreach (var file in convertedContent.Take(10)) // Limit to 10 files max
            {
                sb.AppendLine($"### File: {file.Key}");
                sb.AppendLine($"```{(targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ? "csharp" : "java")}");
                if (file.Value.Length > maxCharsPerFile)
                {
                    sb.AppendLine(file.Value.Substring(0, maxCharsPerFile));
                    sb.AppendLine($"... (truncated, {file.Value.Length - maxCharsPerFile:N0} chars omitted)");
                }
                else
                {
                    sb.AppendLine(file.Value);
                }
                sb.AppendLine("```");
                sb.AppendLine();
            }

            if (convertedContent.Count > 10)
            {
                sb.AppendLine($"... and {convertedContent.Count - 10} more {targetLanguage} files not shown");
                sb.AppendLine();
            }

            // Add validation instructions
            sb.AppendLine("## Validation Requirements");
            sb.AppendLine();
            sb.AppendLine("Please provide a comprehensive validation report with the following structure:");
            sb.AppendLine();
            sb.AppendLine("### 1. ACCURACY SCORE");
            sb.AppendLine("Provide an overall accuracy score from 0-100, where:");
            sb.AppendLine("- 95-100: Fully equivalent, all functionality correctly converted");
            sb.AppendLine("- 80-94: Mostly equivalent, minor differences that don't affect core functionality");
            sb.AppendLine("- 60-79: Partially equivalent, some significant differences but main features work");
            sb.AppendLine("- 40-59: Partially equivalent, major differences requiring fixes");
            sb.AppendLine("- 0-39: Not equivalent, critical functionality missing or incorrect");
            sb.AppendLine();
            sb.AppendLine("Format: **Accuracy Score: XX%**");
            sb.AppendLine();
            sb.AppendLine("### 2. VALIDATION STATUS");
            sb.AppendLine("Classify as one of: FullyEquivalent, MostlyEquivalent, PartiallyEquivalent, NotEquivalent");
            sb.AppendLine("Format: **Status: <STATUS>**");
            sb.AppendLine();
            sb.AppendLine("### 3. FUNCTIONAL ANALYSIS");
            sb.AppendLine("Detailed analysis of:");
            sb.AppendLine("- Data structures and types");
            sb.AppendLine("- Business logic implementation");
            sb.AppendLine("- File I/O and database operations");
            sb.AppendLine("- Error handling");
            sb.AppendLine("- Control flow and program structure");
            sb.AppendLine();
            sb.AppendLine("### 4. DIFFERENCES FOUND");
            sb.AppendLine("List each difference with:");
            sb.AppendLine("- **Severity**: Critical | Major | Moderate | Minor | Info");
            sb.AppendLine("- **Category**: Data Handling | Business Logic | File I/O | Error Handling | etc.");
            sb.AppendLine("- **Description**: What is different");
            sb.AppendLine("- **Expected**: What COBOL does");
            sb.AppendLine("- **Actual**: What converted code does");
            sb.AppendLine("- **Impact**: Effect on functionality");
            sb.AppendLine("- **Fix**: How to correct it");
            sb.AppendLine();
            sb.AppendLine("### 5. CORRECT CONVERSIONS");
            sb.AppendLine("List features that were correctly converted");
            sb.AppendLine();
            sb.AppendLine("### 6. RECOMMENDATIONS");
            sb.AppendLine("Prioritized list of fixes needed, with specific code examples where applicable");
            sb.AppendLine();
            sb.AppendLine("Be specific, thorough, and provide actionable recommendations.");

            return sb.ToString();
        }

        private void ParseAnalysisResult(string analysisResult, ValidationReport report)
        {
            report.DetailedAnalysis = analysisResult;

            // Parse accuracy score
            var accuracyMatch = System.Text.RegularExpressions.Regex.Match(
                analysisResult, @"Accuracy\s*Score:\s*(\d+)%",
                System.Text.RegularExpressions.RegexOptions.IgnoreCase);

            if (accuracyMatch.Success && double.TryParse(accuracyMatch.Groups[1].Value, out var score))
            {
                report.AccuracyScore = score;
            }
            else
            {
                // Default to 50% if not found
                report.AccuracyScore = 50;
            }

            // Parse status
            var statusMatch = System.Text.RegularExpressions.Regex.Match(
                analysisResult, @"Status:\s*(FullyEquivalent|MostlyEquivalent|PartiallyEquivalent|NotEquivalent)",
                System.Text.RegularExpressions.RegexOptions.IgnoreCase);

            if (statusMatch.Success)
            {
                report.Status = Enum.Parse<ValidationStatus>(statusMatch.Groups[1].Value, true);
            }
            else
            {
                // Determine status from accuracy score
                report.Status = report.AccuracyScore switch
                {
                    >= 95 => ValidationStatus.FullyEquivalent,
                    >= 80 => ValidationStatus.MostlyEquivalent,
                    >= 40 => ValidationStatus.PartiallyEquivalent,
                    _ => ValidationStatus.NotEquivalent
                };
            }

            // Parse differences (simplified - the full text analysis is in DetailedAnalysis)
            // In a real implementation, you might parse individual differences more granularly
            ParseDifferences(analysisResult, report);
        }

        private void ParseDifferences(string analysisResult, ValidationReport report)
        {
            // Look for severity markers in the text
            var lines = analysisResult.Split('\n');
            FunctionalDifference? currentDifference = null;

            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();

                // Check for severity indicators
                if (trimmedLine.StartsWith("- **Severity**:", StringComparison.OrdinalIgnoreCase) ||
                    trimmedLine.StartsWith("**Severity**:", StringComparison.OrdinalIgnoreCase))
                {
                    if (currentDifference != null)
                    {
                        report.Differences.Add(currentDifference);
                    }

                    currentDifference = new FunctionalDifference();

                    if (trimmedLine.Contains("Critical", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Severity = DifferenceSeverity.Critical;
                    else if (trimmedLine.Contains("Major", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Severity = DifferenceSeverity.Major;
                    else if (trimmedLine.Contains("Moderate", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Severity = DifferenceSeverity.Moderate;
                    else if (trimmedLine.Contains("Minor", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Severity = DifferenceSeverity.Minor;
                    else
                        currentDifference.Severity = DifferenceSeverity.Info;
                }
                else if (currentDifference != null)
                {
                    if (trimmedLine.StartsWith("- **Category**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Category = ExtractValue(trimmedLine);
                    else if (trimmedLine.StartsWith("- **Description**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Description = ExtractValue(trimmedLine);
                    else if (trimmedLine.StartsWith("- **Expected**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.ExpectedBehavior = ExtractValue(trimmedLine);
                    else if (trimmedLine.StartsWith("- **Actual**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.ActualBehavior = ExtractValue(trimmedLine);
                    else if (trimmedLine.StartsWith("- **Impact**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.Impact = ExtractValue(trimmedLine);
                    else if (trimmedLine.StartsWith("- **Fix**:", StringComparison.OrdinalIgnoreCase))
                        currentDifference.SuggestedFix = ExtractValue(trimmedLine);
                }
            }

            if (currentDifference != null)
            {
                report.Differences.Add(currentDifference);
            }
        }

        private string ExtractValue(string line)
        {
            var colonIndex = line.IndexOf(':');
            if (colonIndex >= 0 && colonIndex < line.Length - 1)
            {
                return line.Substring(colonIndex + 1).Trim('*', ' ');
            }
            return string.Empty;
        }

        private async Task GenerateMarkdownReport(ValidationReport report)
        {
            var sb = new StringBuilder();

            sb.AppendLine($"# {report.TargetLanguage} Conversion Validation Report");
            sb.AppendLine();
            sb.AppendLine($"**Generated:** {report.ValidationTimestamp:yyyy-MM-dd HH:mm:ss} UTC");
            sb.AppendLine();

            // Summary Section
            sb.AppendLine("## Validation Summary");
            sb.AppendLine();
            sb.AppendLine($"- **Accuracy Score:** {report.AccuracyScore:F1}%");
            sb.AppendLine($"- **Status:** {report.Status}");
            sb.AppendLine($"- **COBOL Files Analyzed:** {report.CobolFilesAnalyzed}");
            sb.AppendLine($"- **{report.TargetLanguage} Files Analyzed:** {report.ConvertedFilesAnalyzed}");
            sb.AppendLine();

            // Status Indicator
            var statusEmoji = report.Status switch
            {
                ValidationStatus.FullyEquivalent => "✅",
                ValidationStatus.MostlyEquivalent => "✅",
                ValidationStatus.PartiallyEquivalent => "⚠️",
                ValidationStatus.NotEquivalent => "❌",
                _ => "⚠️"
            };

            sb.AppendLine($"{statusEmoji} **{report.Status}** - {GetStatusDescription(report.Status)}");
            sb.AppendLine();

            // Differences Summary
            if (report.Differences.Any())
            {
                var critical = report.Differences.Count(d => d.Severity == DifferenceSeverity.Critical);
                var major = report.Differences.Count(d => d.Severity == DifferenceSeverity.Major);
                var moderate = report.Differences.Count(d => d.Severity == DifferenceSeverity.Moderate);
                var minor = report.Differences.Count(d => d.Severity == DifferenceSeverity.Minor);

                sb.AppendLine("### Issues Found");
                sb.AppendLine();
                if (critical > 0) sb.AppendLine($"- 🔴 **Critical:** {critical}");
                if (major > 0) sb.AppendLine($"- 🟠 **Major:** {major}");
                if (moderate > 0) sb.AppendLine($"- 🟡 **Moderate:** {moderate}");
                if (minor > 0) sb.AppendLine($"- 🟢 **Minor:** {minor}");
                sb.AppendLine();
            }

            // Detailed Analysis
            sb.AppendLine("## Detailed Analysis");
            sb.AppendLine();
            sb.AppendLine(report.DetailedAnalysis);
            sb.AppendLine();

            // Save report
            var directory = Path.GetDirectoryName(report.ReportPath);
            if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }

            await File.WriteAllTextAsync(report.ReportPath, sb.ToString());
            _logger.LogInformation("Validation report saved to: {ReportPath}", report.ReportPath);
        }

        private string GetStatusDescription(ValidationStatus status)
        {
            return status switch
            {
                ValidationStatus.FullyEquivalent => "All functionality correctly converted with full equivalence",
                ValidationStatus.MostlyEquivalent => "Minor differences that don't affect core functionality",
                ValidationStatus.PartiallyEquivalent => "Some significant differences but main features work",
                ValidationStatus.NotEquivalent => "Critical functionality missing or incorrect",
                ValidationStatus.ValidationFailed => "Validation could not be completed",
                _ => "Unknown status"
            };
        }
    }
}
