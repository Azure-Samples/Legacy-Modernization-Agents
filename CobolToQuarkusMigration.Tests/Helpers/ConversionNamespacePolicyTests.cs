using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Left to the model, one estate produced 33 packages under five unrelated roots and re-emitted
// every shared copybook type under each of them. The namespace is therefore assigned, not asked
// for, and the same program must land in the same place on every run.
[Collection("EnvironmentSensitive")]
public class ConversionNamespacePolicyTests : IDisposable
{
    private readonly string? _original =
        Environment.GetEnvironmentVariable(ConversionNamespacePolicy.RootVariable);

    private static void SetRoot(string? value) =>
        Environment.SetEnvironmentVariable(ConversionNamespacePolicy.RootVariable, value);

    private static void SetArchitecture(string? value) =>
        Environment.SetEnvironmentVariable(ConversionNamespacePolicy.ArchitectureVariable, value);

    private static void SetTemplate(string? value) =>
        Environment.SetEnvironmentVariable(ConversionNamespacePolicy.TemplateVariable, value);

    private readonly string? _originalArchitecture =
        Environment.GetEnvironmentVariable(ConversionNamespacePolicy.ArchitectureVariable);

    private readonly string? _originalTemplate =
        Environment.GetEnvironmentVariable(ConversionNamespacePolicy.TemplateVariable);

    public void Dispose()
    {
        SetRoot(_original);
        SetArchitecture(_originalArchitecture);
        SetTemplate(_originalTemplate);
    }

    [Fact]
    public void UsesALanguageAppropriateRootWhenNoneIsConfigured()
    {
        SetRoot(null);

        ConversionNamespacePolicy.Root("Java").Should().Be("com.modernized");
        ConversionNamespacePolicy.Root("C#").Should().Be("Modernized");
    }

    [Theory]
    [InlineData("C#")]
    [InlineData("CSharp")]
    public void RecognisesBothSpellingsOfTheDotnetTarget(string language)
    {
        ConversionNamespacePolicy.IsCSharp(language).Should().BeTrue();
    }

    [Fact]
    public void CasesTheConfiguredRootForTheTargetLanguage()
    {
        SetRoot("bankdata.core");

        ConversionNamespacePolicy.Root("Java").Should().Be("bankdata.core");
        ConversionNamespacePolicy.Root("C#").Should().Be("Bankdata.Core");
    }

    [Fact]
    public void TheFolderInTheSourceDropNamesTheService()
    {
        ConversionNamespacePolicy.ForProgram("Java", "bd/BDSDA23.cbl").Should().Be("com.modernized.bd");
        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl").Should().Be("Modernized.Bd");
    }

    [Fact]
    public void AProgramInTheSourceRootStillGetsAService()
    {
        ConversionNamespacePolicy.ForProgram("Java", "LONE.cbl").Should().Be("com.modernized.core");
        ConversionNamespacePolicy.ForProgram("C#", null).Should().Be("Modernized.Core");
    }

    // FUENTES/SRC/X.cbl is the SRC area of FUENTES. Naming the service "src" says nothing about
    // it, and every estate laid out that way would collapse into one service.
    [Fact]
    public void AGenericContainerDirectoryYieldsToItsParent()
    {
        ConversionNamespacePolicy.ForProgram("Java", "FUENTES/SRC/KYGHG011.cbl")
            .Should().Be("com.modernized.fuentes");
        ConversionNamespacePolicy.ForProgram("C#", "FUENTES/src/KYGHG011.cbl")
            .Should().Be("Modernized.Fuentes");
    }

    [Fact]
    public void SharedTypesLiveOutsideEveryServiceSoTheyExistOnlyOnce()
    {
        ConversionNamespacePolicy.ForSharedTypes("Java").Should().Be("com.modernized.shared");
        ConversionNamespacePolicy.ForSharedTypes("C#").Should().Be("Modernized.Shared");

        ConversionNamespacePolicy.ForSharedTypes("Java")
            .Should().NotBe(ConversionNamespacePolicy.ForProgram("Java", "bd/BDSDA23.cbl"));
    }

    [Fact]
    public void ProgramsInTheSameFolderShareANamespace()
    {
        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl")
            .Should().Be(ConversionNamespacePolicy.ForProgram("C#", "bd/RGNB649.cbl"));
    }

    [Fact]
    public void ProgramsInDifferentFoldersDoNot()
    {
        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl")
            .Should().NotBe(ConversionNamespacePolicy.ForProgram("C#", "FUENTES/KYGFR002.cbl"));
    }

    [Theory]
    [InlineData("my service", "Modernized.MyService")]
    [InlineData("pay-ments", "Modernized.PayMents")]
    public void FoldersThatAreNotValidIdentifiersAreMadeSafe(string folder, string expected)
    {
        ConversionNamespacePolicy.ForProgram("C#", $"{folder}/P.cbl").Should().Be(expected);
    }

    [Fact]
    public void JavaKeepsTheSeparatorItCanUseRatherThanCasingTheSegment()
    {
        ConversionNamespacePolicy.ForProgram("Java", "my service/P.cbl")
            .Should().Be("com.modernized.my_service");
    }

    [Fact]
    public void ASegmentNeverOpensWithADigit()
    {
        ConversionNamespacePolicy.ForProgram("Java", "2024/P.cbl").Should().Be("com.modernized._2024");
    }

    [Fact]
    public void AWindowsPathNamesTheSameServiceAsAPosixOne()
    {
        ConversionNamespacePolicy.ForProgram("C#", @"bd\BDSDA23.cbl").Should().Be("Modernized.Bd");
    }

    [Fact]
    public void AnUnusableConfiguredRootFallsBackRatherThanEmittingAnInvalidNamespace()
    {
        SetRoot("...");
        ConversionNamespacePolicy.Root("C#").Should().Be("Modernized");
    }

    // ── Choosing a layout ────────────────────────────────────────────────
    //
    // Estates arrive with a house style already decided. Imposing one structure means the output
    // has to be reorganised by hand before it can be merged into anything.

    [Fact]
    public void AnUnsetArchitectureKeepsTheLayoutThatWasHereBefore()
    {
        SetArchitecture(null);

        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl").Should().Be("Modernized.Bd");
        ConversionNamespacePolicy.ForSharedTypes("C#").Should().Be("Modernized.Shared");
    }

    [Theory]
    [InlineData("ddd")]
    [InlineData("layered")]
    [InlineData("DDD")]
    public void ALayeredEstatePutsRecordsInTheDomainAndProgramsInTheApplication(string architecture)
    {
        SetArchitecture(architecture);

        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl")
            .Should().Be("Modernized.Bd.Application");
        ConversionNamespacePolicy.ForSharedTypes("C#").Should().Be("Modernized.Domain");
    }

    [Fact]
    public void ALayeredEstateReadsCorrectlyInJavaToo()
    {
        SetArchitecture("ddd");

        ConversionNamespacePolicy.ForProgram("Java", "bd/BDSDA23.cbl")
            .Should().Be("com.modernized.bd.application");
        ConversionNamespacePolicy.ForSharedTypes("Java").Should().Be("com.modernized.domain");
    }

    [Fact]
    public void AFlatEstatePutsEverythingInOneNamespace()
    {
        SetArchitecture("flat");

        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl").Should().Be("Modernized");
        ConversionNamespacePolicy.ForSharedTypes("C#").Should().Be("Modernized");
    }

    [Fact]
    public void AnEstateCanSupplyItsOwnShape()
    {
        SetArchitecture("custom");
        SetTemplate("{root}.services.{service}.impl");

        ConversionNamespacePolicy.ForProgram("Java", "bd/BDSDA23.cbl")
            .Should().Be("com.modernized.services.bd.impl");
    }

    [Fact]
    public void ACustomShapeWithNoTemplateFallsBackRatherThanEmittingNothing()
    {
        SetArchitecture("custom");
        SetTemplate(null);

        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl").Should().Be("Modernized.Bd");
    }

    // A typo in the architecture name should not fail a conversion that is otherwise fine.
    [Fact]
    public void AnUnrecognisedArchitectureFallsBackToTheDefault()
    {
        SetArchitecture("hexagonal-ish");

        ConversionNamespacePolicy.ForProgram("C#", "bd/BDSDA23.cbl").Should().Be("Modernized.Bd");
    }

    [Fact]
    public void AConfiguredRootSurvivesEveryLayout()
    {
        SetRoot("bankdata.core");

        foreach (var architecture in new[] { "service", "ddd", "flat" })
        {
            SetArchitecture(architecture);
            ConversionNamespacePolicy.ForProgram("C#", "bd/X.cbl")
                .Should().StartWith("Bankdata.Core", $"architecture '{architecture}' must honour the root");
        }
    }

    [Fact]
    public void ALayeredEstateStillSeparatesSharedTypesFromPrograms()
    {
        SetArchitecture("ddd");

        ConversionNamespacePolicy.ForSharedTypes("C#")
            .Should().NotBe(ConversionNamespacePolicy.ForProgram("C#", "bd/X.cbl"));
    }
}
