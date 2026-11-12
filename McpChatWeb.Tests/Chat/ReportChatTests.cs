using System;
using System.IO;
using System.Linq;
using McpChatWeb.Models;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Chat;

// Chat with report used to drop the report and answer from the newest run's statistics, so a
// question about one program came back as a run summary. These pin the report path: the answer
// prompt carries the question, the report sections that match it, and nothing about runs.
public class ReportChatTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), "report-chat-" + Guid.NewGuid().ToString("N"));

    public ReportChatTests() => Directory.CreateDirectory(Path.Combine(_root, "output"));

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); } catch (IOException) { }
    }

    private static string Report(int fillerPerSection = 0)
    {
        var filler = new string('x', fillerPerSection);
        return string.Join("\n", new[]
        {
            "# Reverse Engineering Report",
            "Overview of the estate.",
            "## ORDMAIN",
            "Order entry. Calls PAYRB005 to price the order. " + filler,
            "## PAYRB005",
            "Payment rules: rejects amounts above the credit limit. Uses copybook PAYREC. " + filler,
            "### Business rules",
            "Credit limit is checked before posting. " + filler,
            "## NITEJ001",
            "Nightly batch job that reconciles ledgers. " + filler,
            "```",
            "# not a heading inside a fence",
            "```",
        });
    }

    [Fact]
    public void Split_follows_headings_and_ignores_hashes_in_code_fences()
    {
        var sections = ReportChat.Split(Report());

        Assert.Equal(
            new[] { "Reverse Engineering Report", "ORDMAIN", "PAYRB005", "Business rules", "NITEJ001" },
            sections.Select(s => s.Heading));
        Assert.Equal("Reverse Engineering Report > PAYRB005 > Business rules", sections[3].Breadcrumb);
        Assert.Contains("# not a heading inside a fence", sections[4].Text);
    }

    [Fact]
    public void Select_prefers_the_section_named_in_the_question()
    {
        var sections = ReportChat.Split(Report(fillerPerSection: 2_000));
        var picked = ReportChat.Select(sections, "What does PAYRB005 check?", null, budgetChars: 2_200);

        Assert.Single(picked);
        Assert.Equal("PAYRB005", picked[0].Heading);
    }

    [Fact]
    public void Select_uses_the_previous_question_for_a_follow_up()
    {
        var sections = ReportChat.Split(Report(fillerPerSection: 2_000));
        var history = new[]
        {
            new ChatHistoryMessage("user", "What does NITEJ001 do?"),
            new ChatHistoryMessage("assistant", "It reconciles ledgers nightly."),
        };

        var picked = ReportChat.Select(sections, "When does it run?", history, budgetChars: 2_200);

        Assert.Equal("NITEJ001", Assert.Single(picked).Heading);
    }

    [Fact]
    public void Select_keeps_report_order_and_stays_within_budget()
    {
        var sections = ReportChat.Split(Report(fillerPerSection: 1_000));
        var picked = ReportChat.Select(sections, "PAYRB005 ORDMAIN", null, budgetChars: 2_300);

        Assert.True(picked.Sum(s => s.Text.Length) <= 2_300);
        Assert.Equal(picked.OrderBy(s => s.Order).Select(s => s.Order), picked.Select(s => s.Order));
    }

    [Fact]
    public void Prompt_for_a_large_report_has_the_question_and_rule_and_only_matching_sections()
    {
        var options = new PortalChatOptions { WholeReportMaxChars = 1_000, SectionBudgetChars = 2_200 };
        var prompt = ReportChat.BuildPrompt("re.md", Report(fillerPerSection: 2_000), "What does PAYRB005 check?", null, options);

        Assert.Contains(ReportChat.AnswerOnlyRule, prompt);
        Assert.Contains("QUESTION: What does PAYRB005 check?", prompt);
        Assert.Contains("--- SECTION: Reverse Engineering Report > PAYRB005 ---", prompt);
        Assert.DoesNotContain("--- SECTION: Reverse Engineering Report > NITEJ001 ---", prompt);
        // The outline still names every section, so the model can point elsewhere.
        Assert.Contains("- Reverse Engineering Report > NITEJ001", prompt);
    }

    [Fact]
    public void Prompt_for_a_small_report_sends_it_whole()
    {
        var report = Report();
        var prompt = ReportChat.BuildPrompt("re.md", report, "Which job reconciles ledgers?", null, new PortalChatOptions());

        Assert.Contains(report, prompt);
        Assert.DoesNotContain("OUTLINE:", prompt);
    }

    [Fact]
    public void History_is_capped_and_skips_notices()
    {
        var options = new PortalChatOptions { MaxHistoryMessages = 2, MaxHistoryMessageChars = 5 };
        var history = new[]
        {
            new ChatHistoryMessage("user", "first question"),
            new ChatHistoryMessage("system", "Answering from the report"),
            new ChatHistoryMessage("assistant", "first answer"),
            new ChatHistoryMessage("user", "second"),
        };

        var block = ReportChat.FormatHistory(history, options);

        Assert.DoesNotContain("first question", block);
        Assert.DoesNotContain("Answering from the report", block);
        Assert.Contains("Assistant: first…", block);
        Assert.Contains("User: secon…", block);
    }

    [Fact]
    public void Latest_resolves_to_the_newest_report()
    {
        var older = Path.Combine(_root, "output", "reverse-engineering-old.md");
        var newer = Path.Combine(_root, "output", "run-2", "reverse-engineering-new.md");
        Directory.CreateDirectory(Path.GetDirectoryName(newer)!);
        File.WriteAllText(older, "# old");
        File.WriteAllText(newer, "# new");
        File.SetLastWriteTimeUtc(older, DateTime.UtcNow.AddHours(-2));
        File.SetLastWriteTimeUtc(newer, DateTime.UtcNow);

        Assert.Equal(Path.GetFullPath(newer), ReportCatalog.Resolve(_root, "latest"));
    }

    [Fact]
    public void Latest_prefers_a_reverse_engineering_report_over_a_newer_migration_report()
    {
        var re = Path.Combine(_root, "output", "reverse-engineering-details.md");
        var migration = Path.Combine(_root, "output", "java", "migration-report.md");
        Directory.CreateDirectory(Path.GetDirectoryName(migration)!);
        File.WriteAllText(re, "# re");
        File.WriteAllText(migration, "# migration");
        File.SetLastWriteTimeUtc(re, DateTime.UtcNow.AddHours(-2));
        File.SetLastWriteTimeUtc(migration, DateTime.UtcNow);

        Assert.Equal(Path.GetFullPath(re), ReportCatalog.Resolve(_root, "latest"));
    }

    [Fact]
    public void List_does_not_follow_directory_symlinks_back_into_output()
    {
        var report = Path.Combine(_root, "output", "scan-1", "reverse-engineering-details.md");
        Directory.CreateDirectory(Path.GetDirectoryName(report)!);
        File.WriteAllText(report, "# re");
        Directory.CreateSymbolicLink(Path.Combine(_root, "output", "scan-1", "output-link"), Path.Combine(_root, "output"));

        var reports = ReportCatalog.List(_root);

        Assert.Equal(Path.Combine("output", "scan-1", "reverse-engineering-details.md"), Assert.Single(reports).Path);
    }

    [Fact]
    public void Only_listed_reports_resolve_and_linked_reports_are_refused()
    {
        var report = Path.Combine(_root, "output", "reverse-engineering-details.md");
        Directory.CreateDirectory(Path.GetDirectoryName(report)!);
        File.WriteAllText(report, "# re");
        File.WriteAllText(Path.Combine(_root, "appsettings.json"), "{ \"secret\": 1 }");
        File.WriteAllText(Path.Combine(_root, "output", "notes.md"), "# not a report");
        var outside = Path.Combine(Path.GetTempPath(), "lma-outside-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(outside);
        try
        {
            File.WriteAllText(Path.Combine(outside, "reverse-engineering-x.md"), "# outside");
            Directory.CreateSymbolicLink(Path.Combine(_root, "output", "linked"), outside);

            Assert.Equal(Path.GetFullPath(report), ReportCatalog.Resolve(_root, "output/reverse-engineering-details.md"));
            Assert.Null(ReportCatalog.Resolve(_root, "appsettings.json"));
            Assert.Null(ReportCatalog.Resolve(_root, "output/notes.md"));
            Assert.Null(ReportCatalog.Resolve(_root, "output/linked/reverse-engineering-x.md"));
        }
        finally
        {
            Directory.Delete(outside, recursive: true);
        }
    }

    [Fact]
    public void Latest_refuses_a_standard_report_that_is_a_link()
    {
        var outside = Path.Combine(Path.GetTempPath(), "lma-outside-" + Guid.NewGuid().ToString("N") + ".md");
        File.WriteAllText(outside, "# outside");
        try
        {
            Directory.CreateDirectory(Path.Combine(_root, "output"));
            File.CreateSymbolicLink(Path.Combine(_root, "output", "reverse-engineering-details.md"), outside);

            Assert.Null(ReportCatalog.Resolve(_root, "latest"));
        }
        finally
        {
            File.Delete(outside);
        }
    }

    [Fact]
    public void Paths_outside_the_repository_are_refused()
    {
        Assert.Null(ReportCatalog.Resolve(_root, "../outside.md"));
        Assert.Null(ReportCatalog.Resolve(_root, "latest")); // no reports yet
    }

    [Fact]
    public void Options_come_from_the_PortalChat_section()
    {
        var path = Path.Combine(_root, "appsettings.json");
        File.WriteAllText(path, """{ "PortalChat": { "_description": "x", "SectionBudgetChars": 1234, "MaxSavedConversations": 7 } }""");

        var options = PortalChatOptions.Load(path, out var warning);

        Assert.Null(warning);
        Assert.Equal(1234, options.SectionBudgetChars);
        Assert.Equal(7, options.MaxSavedConversations);
        Assert.Equal(new PortalChatOptions().WholeReportMaxChars, options.WholeReportMaxChars);
    }
    [Fact]
    public void Latest_resolves_to_the_report_present_at_question_time()
    {
        var older = Path.Combine(_root, "output", "reverse-engineering-details.md");
        File.WriteAllText(older, "# old");
        File.SetLastWriteTimeUtc(older, DateTime.UtcNow.AddHours(-1));
        Assert.Equal("output/reverse-engineering-details.md",
            ReportCatalog.RelativePath(_root, ReportCatalog.Resolve(_root, "latest")!));

        var newer = Path.Combine(_root, "output", "run-2", "reverse-engineering-report.md");
        Directory.CreateDirectory(Path.GetDirectoryName(newer)!);
        File.WriteAllText(newer, "# new");
        Assert.Equal("output/run-2/reverse-engineering-report.md",
            ReportCatalog.RelativePath(_root, ReportCatalog.Resolve(_root, "latest")!));
    }
}
