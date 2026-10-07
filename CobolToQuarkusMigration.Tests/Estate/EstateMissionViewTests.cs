using CobolToQuarkusMigration.Estate;
using FluentAssertions;
using Xunit;
using static CobolToQuarkusMigration.Tests.Estate.EstateSourceScannerTests;

namespace CobolToQuarkusMigration.Tests.Estate;

public sealed class EstateMissionViewTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("estate-mission-").FullName;

    public void Dispose() => Directory.Delete(_root, recursive: true);

    private void Write(string relativePath, string text)
    {
        var path = Path.Join(_root, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text);
    }

    // A small online estate defined the way newer CICS samples do it: YAML resource definitions,
    // DDL for the tables and z/OS Connect style API operations.
    private EstateGraph ShopEstate()
    {
        Write("shop/cobol/ORDMAIN.cbl", Cobol(
            "*  FUNCTION: Takes customer orders over CICS.",
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. ORDMAIN.",
            "PROCEDURE DIVISION.",
            "MAIN-PARA.",
            "    EXEC CICS LINK PROGRAM('PRICING') END-EXEC",
            "    IF WS-A = WS-B",
            "       PERFORM SUB-PARA",
            "    END-IF.",
            "SUB-PARA.",
            "    EXEC CICS RETURN END-EXEC."));
        Write("shop/cobol/PRICING.cbl", Cobol(
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. PRICING.",
            "PROCEDURE DIVISION.",
            "    CALL 'CEE3ABD'.",
            "    GOBACK."));
        Write("shop/cobol/ORPHAN.cbl", Cobol("PROGRAM-ID. ORPHAN.", "PROCEDURE DIVISION.", "    GOBACK."));
        Write("shop/cics/shop-definitions.yaml", string.Join("\n",
            "resourceDefinitions:",
            "  - file:",
            "      name: ORDFILE",
            "      description: Orders VSAM",
            "      dsname: SHOP.ORDERS.KSDS",
            "  - transaction:",
            "      name: ordr",
            "      program: ORDMAIN",
            "      description: Order entry",
            "  - program:",
            "      name: PRICING", ""));
        Write("shop/ddl/prices.ddl", "CREATE TABLE SHOP.PRICES (ID INTEGER);\n");
        Write("shop/api/zosAssets/PriceAsset/zosAsset.yaml", "assetType: CICS\nprogram: pricing\n");
        Write("shop/api/operations/%2Fprices%2F%7Bid%7D/get/operation.yaml", "operationId: getPrice\nzasset: PriceAsset\n");
        return EstateGraphBuilder.Build(_root);
    }

    private static EstateEdge Edge(EstateGraph g, string from, string to, string kind) =>
        g.Edges.Single(e => e.From == from && e.To == to && e.Kind == kind);

    [Fact]
    public void CicsYamlDefinesTransactionsAndFilesWithTheirDatasets()
    {
        var g = ShopEstate();

        var run = Edge(g, "transaction:ORDR", "program:ORDMAIN", EstateEdgeKind.Runs);
        run.Evidence.Single().File.Should().Be("shop/cics/shop-definitions.yaml");
        g.Nodes.Single(n => n.Id == "transaction:ORDR").Attributes["description"].Should().Be("Order entry");
        Edge(g, "file:ORDFILE", "dataset:SHOP.ORDERS.KSDS", EstateEdgeKind.BackedBy);
    }

    [Fact]
    public void DdlTablesAreInSource()
    {
        var g = ShopEstate();

        g.Nodes.Should().Contain(n => n.Id == "table:SHOP.PRICES" && n.InSource && n.File == "shop/ddl/prices.ddl");
    }

    [Fact]
    public void AnApiOperationInvokesTheProgramBehindItsAsset()
    {
        var g = ShopEstate();

        var api = g.Nodes.Single(n => n.Kind == EstateNodeKind.Api);
        api.Name.Should().Be("GET /prices/{id}");
        api.Attributes["asset"].Should().Be("PRICEASSET");
        Edge(g, api.Id, "program:PRICING", EstateEdgeKind.Invokes);
        g.Nodes.Should().NotContain(n => n.Kind == EstateNodeKind.Transaction && n.File != null && n.File.Contains("api/"));
    }

    [Fact]
    public void MetricsCountShapeAndDescriptionPrefersTheFunctionLine()
    {
        var src = Cobol(
            "*  FUNCTION: Takes customer orders over CICS.",
            "PROGRAM-ID. ORDMAIN.",
            "PROCEDURE DIVISION.",
            "MAIN-PARA.",
            "    EXEC CICS LINK PROGRAM('PRICING') END-EXEC",
            "    EXEC SQL SELECT 1 INTO :X FROM T END-EXEC",
            "    IF A = B PERFORM SUB-PARA END-IF",
            "    EVALUATE TRUE WHEN A PERFORM SUB-PARA UNTIL B END-EVALUATE",
            "    GO TO MAIN-EXIT.",
            "SUB-PARA.",
            "    CONTINUE.",
            "MAIN-EXIT SECTION.",
            "    EXIT.");

        var m = EstateProgramMetrics.Measure(src);

        m["paragraphs"].Should().Be(3);
        m["complexity"].Should().Be(4);
        m["perform"].Should().Be(2);
        m["goto"].Should().Be(1);
        m["execCics"].Should().Be(1);
        m["execSql"].Should().Be(1);
        m["execDli"].Should().Be(0);
        m["lines"].Should().Be(13);
        EstateProgramMetrics.Description(src).Should().Be("Takes customer orders over CICS.");
    }

    [Fact]
    public void DescriptionSkipsLicenceBanners()
    {
        var src = Cobol(
            "* Copyright Example Corp. All rights reserved.",
            "* Licensed under the Apache License, Version 2.0",
            "* Prints the monthly account statement for each customer.",
            "PROGRAM-ID. STMT.");

        EstateProgramMetrics.Description(src).Should().Be("Prints the monthly account statement for each customer.");
    }

    [Fact]
    public void ProjectionGivesKindsTechStatusAndEntryPoints()
    {
        var g = ShopEstate();
        var status = new Dictionary<string, EstateMissionStatus>
        {
            ["program:ORDMAIN"] = new(true, "full", new Dictionary<string, EstateMissionConversion>
            {
                ["Java"] = new("Evaluated", 0.95, 0.75),
                ["C#"] = new("Evaluated", 0.6, 0.75),
            }),
        };

        var doc = EstateMissionView.Build(g, status, new EstateGraphOptions());
        EstateMissionNode Node(string id) => doc.Nodes.Single(n => n.Id == id);

        Node("program:ORDMAIN").Kind.Should().Be("online");
        Node("program:ORDMAIN").Tech.Should().Contain("CICS");
        Node("program:ORDMAIN").Estate.Should().Be("shop");
        Node("program:ORDMAIN").Status!.Rekt.Should().BeTrue();
        Node("program:ORDMAIN").Status!.BestParity.Should().Be(0.95);
        Node("program:ORDMAIN").Description.Should().Be("Takes customer orders over CICS.");
        Node("program:ORDMAIN").Reachable.Should().BeTrue();
        Node("program:PRICING").Kind.Should().Be("api");
        Node("program:ORPHAN").Flags.Should().Contain("unreferenced");
        Node("program:ORPHAN").Reachable.Should().BeFalse();
        Node("program:CEE3ABD").Type.Should().Be("utility");
        Node("dataset:SHOP.ORDERS.KSDS").Type.Should().Be("dataset");
        Node("table:SHOP.PRICES").Store.Should().Be("DB2 table");

        doc.Edges.Should().Contain(e => e.Source == "transaction:ORDR" && e.Target == "program:ORDMAIN" && e.Type == "starts");
        doc.Edges.Should().Contain(e => e.Source == "program:ORDMAIN" && e.Target == "program:PRICING" && e.Type == "calls");
        doc.Edges.Should().Contain(e => e.Type == "invokes" && e.Target == "program:PRICING");
        doc.Kpis["apis"].Should().Be(1);
        doc.Kpis["transactions"].Should().Be(1);
        doc.Kpis["converted"].Should().Be(1);
        doc.Estates.Single().Id.Should().Be("shop");
        doc.Estates.Single().Apis.Should().Be(1);
    }

    [Fact]
    public void ClustersListEntryPointsTierAndIgnoreSystemRoutinesAsMissing()
    {
        var g = ShopEstate();

        var doc = EstateMissionView.Build(g, new Dictionary<string, EstateMissionStatus>(), new EstateGraphOptions());

        var c = doc.Clusters.Single(x => x.Members.Contains("program:ORDMAIN"));
        c.Members.Should().Contain("program:PRICING");
        c.EntryPoints.Should().Contain("transaction:ORDR");
        c.EntryPoints.Should().Contain(x => x.StartsWith("api:"));
        c.Missing.Should().NotContain("CEE3ABD");
        c.SliceId.Should().Be(c.Id);
        c.Tier.Should().BeOneOf("low-risk", "moderate", "core");
        c.Rationale.Should().Contain("entry point");
    }

    [Fact]
    public void DataTouchedByOneClusterIsOwnedAndByTwoIsShared()
    {
        Write("a/cobol/AWRITE.cbl", Cobol(
            "PROGRAM-ID. AWRITE.",
            "PROCEDURE DIVISION.",
            "    EXEC SQL UPDATE BOTH SET X = 1 END-EXEC",
            "    EXEC SQL UPDATE ONLYA SET X = 1 END-EXEC",
            "    CALL 'AHELP'."));
        Write("a/cobol/AHELP.cbl", Cobol("PROGRAM-ID. AHELP.", "PROCEDURE DIVISION.", "    CALL 'AWRITE'."));
        Write("b/cobol/BREAD.cbl", Cobol(
            "PROGRAM-ID. BREAD.",
            "PROCEDURE DIVISION.",
            "    EXEC SQL SELECT X INTO :Y FROM BOTH END-EXEC",
            "    CALL 'BHELP'."));
        Write("b/cobol/BHELP.cbl", Cobol("PROGRAM-ID. BHELP.", "PROCEDURE DIVISION.", "    CALL 'BREAD'."));
        var g = EstateGraphBuilder.Build(_root);

        var doc = EstateMissionView.Build(g, new Dictionary<string, EstateMissionStatus>(), new EstateGraphOptions());

        var a = doc.Clusters.Single(x => x.Members.Contains("program:AWRITE"));
        var b = doc.Clusters.Single(x => x.Members.Contains("program:BREAD"));
        a.Id.Should().NotBe(b.Id);
        a.OwnedData.Should().Contain("table:ONLYA");
        a.SharedData.Should().Contain(s => s.Id == "table:BOTH" && s.Clusters.Contains(b.Id));
    }

    [Fact]
    public void DomainRulesComeFromOptions()
    {
        var g = ShopEstate();
        var options = new EstateGraphOptions
        {
            DomainRules = [new EstateDomainRule { Domain = "Ordering", Name = "^ORD" }],
            DomainFallback = "Unsorted",
        };

        var doc = EstateMissionView.Build(g, new Dictionary<string, EstateMissionStatus>(), options);

        doc.Nodes.Single(n => n.Id == "program:ORDMAIN").Domain.Should().Be("Ordering");
        doc.Nodes.Single(n => n.Id == "program:ORPHAN").Domain.Should().Be("Unsorted");
        doc.Nodes.Single(n => n.Id == "transaction:ORDR").Domain.Should().Be("Ordering");
    }

    [Fact]
    public void TheShippedConfigMatchesTheCodeDefaults()
    {
        var path = Path.Join(AppContext.BaseDirectory, "Config", "appsettings.json");
        if (!File.Exists(path)) return; // Config/ is copied to the output only when present

        var options = EstateGraphOptions.Load(path, out var warning);

        warning.Should().BeNull();
        var defaults = new EstateGraphOptions();
        options.DomainRules.Should().BeEquivalentTo(defaults.DomainRules, o => o.WithStrictOrdering());
        options.SystemProgramPrefixes.Should().Equal(defaults.SystemProgramPrefixes);
        options.LowRiskCarveScore.Should().Be(defaults.LowRiskCarveScore);
        options.ModerateCarveScore.Should().Be(defaults.ModerateCarveScore);
    }
}
