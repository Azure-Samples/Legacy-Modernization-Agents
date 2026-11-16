using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Closed-world COMMAREA slot values across one estate, so a dynamic XCTL/LINK target can be proven finite.
public static partial class RektEvidence
{
    public const string CommareaClosureName = ".commarea-closure.json";

    // True when a fixed-format program that the parser could not export provably sends no COMMAREA.
    private static bool CommareaLexicallyInert(string source)
    {
        List<string> lines;
        try
        {
            lines = PyCompat.SplitLines(PyCompat.ReadText(source, PyCompat.Latin1));
        }
        catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException)
        {
            return false;
        }
        var procedure = false;
        var words = new List<string>();
        foreach (var line in lines)
        {
            if (line.Length >= 7 && line[6] is '*' or '/') continue;
            var area = PyCompat.Slice(line, 7, 72).ToUpperInvariant();
            if (Search("(?<![A-Z0-9-])REPLACE(?![A-Z0-9-])", area)) return false;
            if (Search(@"(?<![A-Z0-9-])PROCEDURE\s+DIVISION(?![A-Z0-9-])", area)) procedure = true;
            if (procedure) words.AddRange(Rx("[A-Z0-9-]+").Matches(area).Select(match => match.Value));
        }
        return procedure && !words.Contains("COMMAREA") && !words.Contains("COPY");
    }

    private sealed record Slot(string Stem, string Record, string Name);

    public static JsonObject CommareaClosure(string sourceRoot, string outputRoot, string estate)
    {
        sourceRoot = PyCompat.Resolve(sourceRoot);
        outputRoot = PyCompat.Resolve(outputRoot);
        var estateRoot = Norm(Path.Join(sourceRoot, estate));
        var errors = new List<string>();
        var participants = new JsonArray();
        var programs = new Dictionary<string, (string RelativePath, string Program)>();
        var inventories = new Dictionary<string, JsonNode>();
        var sources = PyCompat.Walk(estateRoot, name => !name.StartsWith('.'))
            .SelectMany(entry => entry.Files.Where(name => !name.StartsWith('.')).Select(name => Norm(Path.Join(entry.Root, name))))
            .Where(path => File.Exists(path) && PyCompat.Suffix(path).ToLowerInvariant() is ".cbl" or ".cob")
            .Order(PathOrder).ToList();
        foreach (var source in sources)
        {
            var relative = PyCompat.RelPosix(source, sourceRoot);
            var stem = PyCompat.Stem(source).ToUpperInvariant();
            JsonNode? inventory = null, parsedSha = null;
            try
            {
                var export = ReadJson(Path.Join(outputRoot, relative + "-deps.json"));
                var evidence = ReadJson(Path.Join(outputRoot, relative + ".report", "dependency-evidence.json"));
                if (PyJson.PyEquals(Get(evidence, "originalSourceSha256"), PyCompat.Digest(source)) && Get(export, "commareaInventory") is JsonObject found)
                    (inventory, parsedSha) = (found, Get(export, "sourceSha256"));
            }
            catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException or System.Text.Json.JsonException or ArgumentException)
            {
            }
            if (inventory is null)
            {
                if (CommareaLexicallyInert(source))
                {
                    participants.Add(new JsonObject { ["relativePath"] = relative, ["sourceSha256"] = PyCompat.Digest(source), ["mode"] = "lexical" });
                    inventories[stem] = new JsonObject { ["records"] = new JsonArray(), ["carriers"] = new JsonArray(), ["sends"] = new JsonArray() };
                }
                else
                    errors.Add("commarea-closure-participant-unavailable:" + relative);
                continue;
            }
            if (!PyJson.PyEquals(Get(inventory, "schema"), 1) || PyJson.Str(parsedSha) is not { } sha || inventories.ContainsKey(stem))
            {
                errors.Add("commarea-closure-participant-invalid:" + relative);
                continue;
            }
            participants.Add(new JsonObject
            {
                ["relativePath"] = relative,
                ["sourceSha256"] = PyCompat.Digest(source),
                ["mode"] = "inventory",
                ["inventorySha256"] = PyCompat.Digest(System.Text.Encoding.UTF8.GetBytes(PyJson.Dumps(inventory, sortKeys: true))),
            });
            inventories[stem] = inventory;
            programs[sha] = (relative, stem);
        }
        // A null set is Python's "bad" sentinel: the slot can hold values nobody proved.
        var sent = new List<(string Stem, string Record)>();
        var values = new Dictionary<Slot, HashSet<string?>?>();
        var slotOrder = new List<Slot>();
        var layouts = new List<string[]>();
        bool Loaded(string target) => ((JsonArray)Subscript(inventories[target], "records")!).Any(record => PyJson.Truthy(Subscript(record, "loaded")));
        // A LINK whose every proved target loads no COMMAREA record cannot feed any program slot.
        bool LinkedToInert(JsonNode? send) =>
            StrEquals(Subscript(send, "command"), "LINK") && Str(send, "bindingStatus") == "complete"
            && Get(send, "targets") is JsonArray { Count: > 0 } targets
            && targets.All(target => PyJson.Str(target) is { } name && inventories.ContainsKey(name) && !Loaded(name));
        foreach (var (stem, inventory) in inventories)
        {
            var records = new Dictionary<string, JsonNode?>();
            foreach (var record in (JsonArray)Subscript(inventory, "records")!) records[PyJson.Str(Subscript(record, "name"))!] = record;
            var carriers = new Dictionary<string, JsonNode?>();
            foreach (var carrier in (JsonArray)Subscript(inventory, "carriers")!) carriers[PyJson.Str(Subscript(carrier, "name"))!] = carrier;
            foreach (var carrier in carriers.Values)
                if (PyJson.Truthy(Subscript(carrier, "issues")) || !records.ContainsKey(PyJson.Str(Subscript(carrier, "record")) ?? "\0"))
                    errors.Add($"commarea-closure-carrier-unsupported:{stem}:{PyJson.PyStr(Subscript(carrier, "name"))}");
            foreach (var send in (JsonArray)Subscript(inventory, "sends")!)
            {
                var kind = Subscript(send, "kind");
                if (StrEquals(kind, "record"))
                {
                    if (!LinkedToInert(send)) sent.Add((stem, PyJson.Str(Subscript(send, "record"))!));
                }
                else if (StrEquals(kind, "carrier") && PyJson.Str(Subscript(send, "carrier")) is { } carrierName && carriers.ContainsKey(carrierName))
                    sent.Add((stem, PyJson.Str(Subscript(carriers[carrierName], "record"))!));
                else if (StrEquals(kind, "foreign"))
                {
                    var targets = PyJson.Truthy(Get(send, "targets")) ? (JsonArray)Get(send, "targets")! : [];
                    if (targets.Count == 0 || targets.Any(target => PyJson.Str(target) is not { } name || !inventories.ContainsKey(name) || Loaded(name)))
                        errors.Add($"commarea-closure-foreign-send:{stem}:{PyJson.PyStr(Subscript(send, "operand"))}:{PyJson.PyStr(Subscript(send, "line"))}");
                }
                else
                    errors.Add($"commarea-closure-send-unsupported:{stem}:{PyJson.PyStr(Subscript(send, "operand"))}");
            }
            foreach (var (recordName, record) in records)
            {
                if (PyJson.Truthy(Subscript(record, "loaded")) || sent.Contains((stem, recordName)))
                {
                    var layout = ((JsonArray)Subscript(record, "layout")!).Select(entry => PyJson.Str(entry)!).ToArray();
                    if (!layouts.Any(existing => existing.SequenceEqual(layout))) layouts.Add(layout);
                }
                foreach (var (slotName, item) in (JsonObject)Subscript(record, "slots")!)
                {
                    var itemValues = (JsonArray)Subscript(item, "values")!;
                    var ok = !PyJson.Truthy(Subscript(record, "issues")) && !PyJson.Truthy(Subscript(item, "unsupported"))
                        && itemValues.All(value => value is null || PyJson.Str(value) is { } text && Full("[A-Za-z0-9@$#]{1,8}", text) is not null);
                    var key = new Slot(stem, recordName, slotName);
                    if (!values.ContainsKey(key)) slotOrder.Add(key);
                    values[key] = ok ? itemValues.Select(value => PyJson.Str(value)).ToHashSet() : null;
                }
            }
        }
        // Programs may extend the shared record after its copybook; only the common leading entries align.
        var prefix = layouts.Count > 0 ? layouts.MinBy(layout => layout.Length)! : [];
        if (layouts.Any(layout => !layout.Take(prefix.Length).SequenceEqual(prefix)))
            errors.Add("commarea-closure-incompatible-layouts:" + layouts.Count);
        var shared = prefix.Select(entry => Start("[0-9]+([A-Z0-9-]+)PIC", entry)).OfType<Match>().Select(match => match.Groups[1].Value).ToHashSet();
        var changed = true;
        while (changed && errors.Count == 0)
        {
            changed = false;
            var universe = new Dictionary<string, HashSet<string?>?>();
            foreach (var (stem, record) in sent)
                foreach (var key in slotOrder)
                {
                    if (key.Stem != stem || key.Record != record || !shared.Contains(key.Name)) continue;
                    var current = values[key];
                    var known = universe.TryGetValue(key.Name, out var existing) ? existing : [];
                    universe[key.Name] = current is null || known is null ? null : [.. known, .. current];
                }
            foreach (var (stem, inventory) in inventories)
                foreach (var record in (JsonArray)Subscript(inventory, "records")!)
                {
                    var recordName = PyJson.Str(Subscript(record, "name"))!;
                    foreach (var (slotName, item) in (JsonObject)Subscript(record, "slots")!)
                    {
                        var key = new Slot(stem, recordName, slotName);
                        if (values[key] is not { } own) continue;
                        var merged = new HashSet<string?>(own);
                        var contributions = ((JsonArray)Subscript(item, "copies")!)
                            .Select(copy => values[new Slot(stem, recordName, PyJson.Str(copy)!)]).ToList();
                        if (PyJson.Truthy(Subscript(item, "entry")))
                            contributions.Add(shared.Contains(slotName) ? universe.TryGetValue(slotName, out var known) ? known : [] : null);
                        if (contributions.Any(contribution => contribution is null))
                        {
                            values[key] = null;
                            changed = true;
                            continue;
                        }
                        foreach (var contribution in contributions) merged.UnionWith(contribution!);
                        if (!merged.SetEquals(own))
                        {
                            values[key] = merged;
                            changed = true;
                        }
                    }
                }
        }
        var result = new JsonObject();
        if (errors.Count == 0)
            foreach (var (parsedSha, program) in programs.OrderBy(pair => pair.Value.RelativePath, StringComparer.Ordinal))
            {
                var slots = new JsonObject();
                var duplicate = new HashSet<string>();
                foreach (var record in (JsonArray)Subscript(inventories[program.Program], "records")!)
                    foreach (var (slotName, _) in (JsonObject)Subscript(record, "slots")!)
                    {
                        var current = values[new Slot(program.Program, PyJson.Str(Subscript(record, "name"))!, slotName)];
                        if (slots.ContainsKey(slotName) || duplicate.Contains(slotName))
                        {
                            duplicate.Add(slotName);
                            slots.Remove(slotName);
                        }
                        else if (current is not null)
                            slots[slotName] = new JsonArray(current.OrderBy(value => value is not null).ThenBy(value => value ?? "", StringComparer.Ordinal)
                                .Select(value => (JsonNode?)value).ToArray());
                    }
                result[parsedSha] = new JsonObject { ["relativePath"] = program.RelativePath, ["slots"] = slots };
            }
        return new JsonObject
        {
            ["schema"] = 1,
            ["estate"] = estate,
            ["errors"] = ToArray(errors.Distinct().Order(StringComparer.Ordinal)),
            ["participants"] = participants,
            ["programs"] = result,
        };
    }

    public static JsonObject WriteCommareaClosure(string sourceRoot, string outputRoot, string estate)
    {
        var closure = CommareaClosure(sourceRoot, outputRoot, estate);
        var path = Path.Join(outputRoot, estate, CommareaClosureName);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        PyCompat.AtomicWrite(path, PyJson.Dumps(closure, indent: 2, sortKeys: true) + "\n");
        return closure;
    }

    // Closure files used by this export's slot proofs, or a diagnostic when any proof disagrees with the estate.
    internal static (List<JsonObject> Definitions, string? Problem) CommareaClosureBindings(JsonNode document, string outputRoot, string relativePath)
    {
        var estate = Norm(relativePath).Split('/')[0];
        var closurePath = Path.Join(PyCompat.Resolve(outputRoot), estate, CommareaClosureName);
        var used = Items(document, "dependencyDialectStatements")
            .SelectMany(statement => Items(statement, "resourceBindings"))
            .Where(binding => Str(binding, "kind") == "CICS_PROGRAM" && Get(binding, "targetResolution") is JsonObject resolution
                && Str(resolution, "kind") == "closed-world-commarea-slot")
            .Select(binding => Get(binding, "targetResolution")).ToList();
        if (used.Count == 0) return ([], null);
        JsonNode? closure;
        string closureSha;
        try
        {
            closure = ReadJson(closurePath);
            closureSha = PyCompat.Digest(closurePath);
        }
        catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException or ArgumentException)
        {
            return ([], "commarea-closure-missing");
        }
        var program = Get(GetOr(closure, "programs", new JsonObject()), PyJson.Str(Get(document, "sourceSha256")) ?? "\0");
        if (PyJson.Truthy(Get(closure, "errors")) || !PyJson.Truthy(program) || Str(program, "relativePath") != relativePath)
            return ([], "commarea-closure-program-missing");
        foreach (var proof in used)
            if (Str(proof, "closureSha256") != closureSha
                || !PyJson.PyEquals(Get(proof, "programSourceSha256"), Get(document, "sourceSha256"))
                || !PyJson.PyEquals(Get(proof, "universe"), Get(Subscript(program, "slots"), PyJson.Str(Get(proof, "operand")) ?? "\0")))
                return ([], "commarea-closure-proof-mismatch:" + PyJson.PyStr(Get(proof, "operand")));
        return ([new JsonObject { ["kind"] = "COMMAREA_CLOSURE", ["name"] = estate, ["path"] = closurePath, ["sha256"] = closureSha }], null);
    }
}
