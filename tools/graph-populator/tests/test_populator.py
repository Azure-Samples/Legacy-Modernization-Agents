import sys
import types
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


def _install_populator_import_stubs():
    click = types.ModuleType("click")

    def _decorator_factory(*args, **kwargs):
        def decorator(func):
            return func

        return decorator

    def _group(*args, **kwargs):
        def decorator(func):
            func.command = _decorator_factory
            return func

        return decorator

    click.group = _group
    click.command = _decorator_factory
    click.option = _decorator_factory
    sys.modules.setdefault("click", click)

    neo4j = types.ModuleType("neo4j")

    class GraphDatabase:
        @staticmethod
        def driver(*args, **kwargs):
            raise AssertionError("GraphDatabase.driver should not be used in unit tests")

    class ManagedTransaction:
        pass

    neo4j.GraphDatabase = GraphDatabase
    neo4j.ManagedTransaction = ManagedTransaction
    sys.modules.setdefault("neo4j", neo4j)

    rich = types.ModuleType("rich")
    console_module = types.ModuleType("rich.console")
    progress_module = types.ModuleType("rich.progress")

    class Console:
        def print(self, *args, **kwargs):
            return None

    class _RichStub:
        def __init__(self, *args, **kwargs):
            pass

    console_module.Console = Console
    progress_module.Progress = _RichStub
    progress_module.SpinnerColumn = _RichStub
    progress_module.BarColumn = _RichStub
    progress_module.TextColumn = _RichStub

    sys.modules.setdefault("rich", rich)
    sys.modules.setdefault("rich.console", console_module)
    sys.modules.setdefault("rich.progress", progress_module)


_install_populator_import_stubs()

import tempfile

from populator import batch_merge_relationships, begin_rekt_scan, ingest_rekt_ast, ingest_rekt_data_structures, ingest_rekt_outputs
from source_paths import scoped_graph_id


class BatchMergeRelationshipsTests(unittest.TestCase):
    @patch("populator.console")
    def test_missing_endpoints_are_reported_and_not_counted_as_merged(self, console):
        driver = MagicMock()
        transaction = MagicMock()
        transaction.run.return_value.single.return_value = {"matched": 1}
        session = driver.session.return_value.__enter__.return_value
        session.execute_write.side_effect = lambda action, query, batch: action(transaction, query, batch)

        count = batch_merge_relationships(
            driver, "ASTNode", "uid", "FOLLOWED_BY", "ASTNode", "uid",
            [{"from_id": "start", "to_id": "end"}, {"from_id": "missing", "to_id": "end"}],
        )

        self.assertEqual(1, count)
        self.assertIn("RETURN count(r) AS matched", transaction.run.call_args.args[0])
        transaction.run.return_value.single.assert_called_once_with(strict=True)
        self.assertIn("Skipped 1 FOLLOWED_BY", str(console.print.call_args))
        self.assertIn("missing endpoints", str(console.print.call_args))


class NamedRektScanTests(unittest.TestCase):
    def test_existing_run_is_rejected_without_writing_scan_metadata(self):
        driver = MagicMock()
        transaction = MagicMock()
        transaction.run.return_value.single.return_value = {"occupied": True}
        driver.session.return_value.__enter__.return_value.execute_write.side_effect = lambda action: action(transaction)
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaisesRegex(ValueError, "already contains data"):
                begin_rekt_scan(driver, 7, "snapshot-v2", directory, directory)
        self.assertEqual(1, transaction.run.call_count)

    def test_fresh_scan_is_named_without_creating_a_migration_run(self):
        driver = MagicMock()
        transaction = MagicMock()
        transaction.run.return_value.single.return_value = {"occupied": False}
        driver.session.return_value.__enter__.return_value.execute_write.side_effect = lambda action: action(transaction)
        with tempfile.TemporaryDirectory() as directory:
            begin_rekt_scan(driver, 8, "snapshot-v2", directory, directory)
        create = transaction.run.call_args
        self.assertIn("CREATE (s:RektScan", create.args[0])
        self.assertEqual("snapshot-v2", create.kwargs["name"])
        self.assertEqual(8, create.kwargs["runId"])

    def test_missing_artifact_directory_is_rejected_before_accessing_graph(self):
        driver = MagicMock()
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaisesRegex(ValueError, "existing source and artifact directories"):
                begin_rekt_scan(driver, 8, "snapshot-v2", directory, str(Path(directory) / "absent"))
        driver.session.assert_not_called()


class IngestRektAstTests(unittest.TestCase):
    @patch("populator.batch_merge_relationships")
    @patch("populator.batch_merge_nodes")
    def test_preserves_native_text_verbatim_on_root_and_children(
        self, merge_nodes, merge_relationships,
    ):
        driver = MagicMock()
        text = '  DISPLAY "café".\r\n\tMOVE A TO B.  \n'
        ast = {
            "id": "root", "type": "PROGRAM", "originalText": text,
            "text": "legacy root",
            "children": [
                {"id": "child", "type": "DISPLAY", "originalText": 'DISPLAY "café".'},
            ],
        }
        merge_nodes.return_value = 2

        self.assertEqual(2, ingest_rekt_ast(driver, "demo.cbl", ast, 7))

        nodes = merge_nodes.call_args.args[2]
        self.assertEqual([text, 'DISPLAY "café".'], [node["originalText"] for node in nodes])
        self.assertEqual("uid", merge_nodes.call_args.kwargs["merge_key"])
        self.assertEqual(
            [{"from_id": nodes[0]["uid"], "to_id": nodes[1]["uid"]}],
            merge_relationships.call_args.args[6],
        )
        self.assertEqual(
            nodes[0]["uid"],
            driver.session.return_value.__enter__.return_value.run.call_args.kwargs["rootUid"],
        )

    @patch("populator.batch_merge_relationships")
    @patch("populator.batch_merge_nodes")
    def test_text_fallback_preserves_empty_strings_and_normalizes_null(
        self, merge_nodes, merge_relationships,
    ):
        cases = [
            ({"originalText": "", "text": "legacy"}, ""),
            ({"originalText": " \n", "text": "legacy"}, " \n"),
            ({"originalText": None, "text": " legacy\n"}, " legacy\n"),
            ({"text": " legacy\n"}, " legacy\n"),
            ({"originalText": None, "text": ""}, ""),
            ({"originalText": None, "text": None}, ""),
            ({"originalText": None}, ""),
            ({"text": None}, ""),
            ({}, ""),
        ]
        for fields, expected in cases:
            with self.subTest(fields=fields):
                ingest_rekt_ast(MagicMock(), "demo.cbl", {"id": "root", **fields}, 7)
                self.assertEqual(expected, merge_nodes.call_args.args[2][0]["originalText"])

    @patch("populator.batch_merge_relationships")
    @patch("populator.batch_merge_nodes")
    def test_idless_native_and_legacy_nodes_use_the_same_text_for_identity(
        self, merge_nodes, merge_relationships,
    ):
        for field in ("text", "originalText"):
            ingest_rekt_ast(
                MagicMock(), "demo.cbl", {"type": "DISPLAY", field: "DISPLAY A."}, 7,
            )
        legacy, native = [call.args[2][0] for call in merge_nodes.call_args_list]
        self.assertEqual(legacy["uid"], native["uid"])


class IngestRektDataStructuresTests(unittest.TestCase):
    @patch("populator.batch_merge_relationships")
    @patch("populator.batch_merge_nodes")
    def test_scopes_nodes_and_relationships_by_run_and_program(
        self,
        merge_nodes,
        merge_relationships,
    ):
        merge_nodes.return_value = 4
        merge_relationships.return_value = 0
        ds_json = {
            "records": [
                {
                    "id": "100",
                    "name": "CUSTOMER-REC",
                    "level": 1,
                    "children": [
                        {"id": "200", "name": "CUSTOMER-ID", "level": 5},
                    ],
                },
                {"id": "300", "name": "ORDER-ID", "level": 5},
                {"id": "400", "name": "LEGACY-ORDER-ID", "level": 5},
            ],
            "edges": [
                {"from": "200", "to": "300", "type": "FLOWS_INTO"},
                {"from": "300", "to": "400", "type": "REDEFINES"},
            ],
        }

        count = ingest_rekt_data_structures(
            object(),
            "finance/ACCOUNTS.cbl",
            ds_json,
            7,
        )

        self.assertEqual(4, count)
        self.assertEqual("uid", merge_nodes.call_args.kwargs["merge_key"])

        nodes = merge_nodes.call_args.args[2]
        customer_rec = next(node for node in nodes if node["id"] == "100")
        customer_id = next(node for node in nodes if node["id"] == "200")
        order_id = next(node for node in nodes if node["id"] == "300")
        legacy_order_id = next(node for node in nodes if node["id"] == "400")

        self.assertEqual(
            scoped_graph_id(7, "finance/ACCOUNTS.cbl", "100"),
            customer_rec["uid"],
        )
        self.assertEqual("100", customer_rec["id"])

        contains_call, flows_call, redefines_call = merge_relationships.call_args_list

        self.assertEqual(("DataStructure", "uid"), contains_call.args[1:3])
        self.assertEqual(("DataStructure", "uid"), contains_call.args[4:6])
        self.assertEqual(
            [{"from_id": customer_rec["uid"], "to_id": customer_id["uid"]}],
            contains_call.args[6],
        )

        self.assertEqual(("DataStructure", "uid"), flows_call.args[1:3])
        self.assertEqual(("DataStructure", "uid"), flows_call.args[4:6])
        self.assertEqual(
            [{"from_id": customer_id["uid"], "to_id": order_id["uid"]}],
            flows_call.args[6],
        )

        self.assertEqual(("DataStructure", "uid"), redefines_call.args[1:3])
        self.assertEqual(("DataStructure", "uid"), redefines_call.args[4:6])
        self.assertEqual(
            [{"from_id": order_id["uid"], "to_id": legacy_order_id["uid"]}],
            redefines_call.args[6],
        )

    @patch("populator.batch_merge_relationships")
    @patch("populator.batch_merge_nodes")
    def test_same_upstream_id_generates_distinct_scoped_uids(
        self,
        merge_nodes,
        merge_relationships,
    ):
        merge_nodes.return_value = 1
        merge_relationships.return_value = 0
        ds_json = {"id": "ROOT", "name": "CUSTOMER-REC", "level": 1}

        ingest_rekt_data_structures(object(), "finance/ACCOUNTS.cbl", ds_json, 1)
        ingest_rekt_data_structures(object(), "archive/ACCOUNTS.cbl", ds_json, 1)
        ingest_rekt_data_structures(object(), "finance/ACCOUNTS.cbl", ds_json, 2)

        first_node = merge_nodes.call_args_list[0].args[2][0]
        second_node = merge_nodes.call_args_list[1].args[2][0]
        third_node = merge_nodes.call_args_list[2].args[2][0]

        self.assertEqual("ROOT", first_node["id"])
        self.assertEqual("ROOT", second_node["id"])
        self.assertEqual("ROOT", third_node["id"])
        self.assertNotEqual(first_node["uid"], second_node["uid"])
        self.assertNotEqual(first_node["uid"], third_node["uid"])


class IngestRektOutputsTests(unittest.TestCase):
    @patch("populator.ingest_rekt_cfg")
    @patch("populator.ingest_rekt_ast")
    @patch("populator.create_source_blocks")
    @patch("populator.batch_merge_nodes")
    def test_native_cfg_is_loaded_after_its_ast_endpoints(
        self, merge_nodes, create_blocks, ingest_ast, ingest_cfg
    ):
        calls = MagicMock()
        calls.attach_mock(ingest_ast, "ast")
        calls.attach_mock(ingest_cfg, "cfg")
        ingest_ast.return_value = 2
        ingest_cfg.return_value = 1
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            source.mkdir()
            (source / "ACCOUNTS.cbl").write_text("IDENTIFICATION DIVISION.\n")
            report = root / "output/ACCOUNTS.cbl.report"
            for name, content in (
                ("cfg/cfg-ACCOUNTS.cbl.json", '{"edges":[{"fromNodeID":"start","toNodeID":"end"}]}'),
                ("flow_ast/flow-ast-ACCOUNTS.cbl.json", '{"id":"start","children":[{"id":"end"}]}'),
            ):
                path = report / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(content)
            counts = ingest_rekt_outputs(object(), str(root / "output"), str(source), 8)

        self.assertEqual(["ast", "cfg"], [call[0] for call in calls.mock_calls])
        self.assertEqual(1, counts["CFGEdge"])
        self.assertEqual("ACCOUNTS.cbl", ingest_cfg.call_args.args[1])

    @patch("populator.create_source_blocks")
    @patch("populator.batch_merge_nodes")
    def test_file_metadata_keeps_unresolved_dependency_diagnostics_visible(
        self, merge_nodes, create_blocks
    ):
        merge_nodes.return_value = 1
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            source.mkdir()
            (source / "ACCOUNTS.cbl").write_text("IDENTIFICATION DIVISION.\n")
            report = root / "output/ACCOUNTS.cbl.report"
            report.mkdir(parents=True)
            (report / "dependency-evidence.json").write_text(
                '{"unresolvedDiagnostics":["native:unresolved-copybook:ABSENT"]}'
            )
            ingest_rekt_outputs(object(), str(root / "output"), str(source), 8)
        node = merge_nodes.call_args.args[2][0]
        self.assertTrue(node["dependencyEvidenceAvailable"])
        self.assertEqual(["native:unresolved-copybook:ABSENT"], node["dependencyDiagnostics"])

    @patch("populator.ingest_rekt_ast")
    @patch("populator.create_source_blocks")
    @patch("populator.batch_merge_nodes")
    def test_quarantined_and_work_in_progress_artifacts_are_not_active_graph_evidence(
        self, merge_nodes, create_blocks, ingest_ast
    ):
        merge_nodes.return_value = 1
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            source.mkdir()
            (source / "ACCOUNTS.cbl").write_text("IDENTIFICATION DIVISION.\n")
            output = root / "output"
            for prefix in (".previous/old", ".work/current"):
                report = output / prefix / "ACCOUNTS.cbl.report/flow_ast"
                report.mkdir(parents=True)
                (report / "flow-ast-ACCOUNTS.cbl.json").write_text('{"id":"OLD"}')
            counts = ingest_rekt_outputs(object(), str(output), str(source), 1)
        self.assertEqual(0, counts["ASTNode"])
        ingest_ast.assert_not_called()

    @patch("populator.ingest_rekt_ast")
    @patch("populator.create_source_blocks")
    @patch("populator.batch_merge_nodes")
    def test_null_artifact_is_skipped(
        self,
        merge_nodes,
        create_blocks,
        ingest_ast,
    ):
        merge_nodes.return_value = 1
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source_dir = root / "source"
            source_dir.mkdir()
            (source_dir / "ACCOUNTS.cbl").write_text("IDENTIFICATION DIVISION.\n")

            rekt_dir = root / "rekt"
            report_dir = rekt_dir / "ACCOUNTS.cbl.report"
            report_dir.mkdir(parents=True)
            (report_dir / "flow-ast-ACCOUNTS.cbl.json").write_text("null")

            counts = ingest_rekt_outputs(object(), str(rekt_dir), str(source_dir), 1)

        ingest_ast.assert_not_called()
        self.assertEqual(0, counts["ASTNode"])


    @patch("populator.console")
    @patch("populator.create_source_blocks")
    @patch("populator.batch_merge_nodes")
    def test_facts_and_parser_diagnostics_are_not_treated_as_parse_artifacts(
        self,
        merge_nodes,
        create_blocks,
        console,
    ):
        merge_nodes.return_value = 1
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source_dir = root / "source"
            source_dir.mkdir()
            (source_dir / "ACCOUNTS.cbl").write_text("IDENTIFICATION DIVISION.\n")

            rekt_dir = root / "rekt"
            rekt_dir.mkdir()
            (rekt_dir / "ACCOUNTS.cbl.facts.json").write_text("{}")
            report_dir = rekt_dir / "ACCOUNTS.cbl.report"
            report_dir.mkdir()
            (report_dir / "parse-diagnostics.json").write_text(
                '[{"code":"missing copybook","severity":"ERROR","message":"ABSENT"}]'
            )

            counts = ingest_rekt_outputs(object(), str(rekt_dir), str(source_dir), 1)

        printed = " ".join(str(call) for call in console.print.call_args_list)
        self.assertNotIn("ambiguous source", printed)
        self.assertNotIn("degenerate parse", printed)
        self.assertEqual(0, counts["ASTNode"])
        self.assertEqual(0, counts["CFGEdge"])
        self.assertEqual(0, counts["DataStructure"])


class SchemaTests(unittest.TestCase):
    def test_datastructure_schema_migrates_to_uid_constraint(self):
        schema_text = (
            Path(__file__).resolve().parents[1] / "schema.cypher"
        ).read_text()

        self.assertIn("DROP CONSTRAINT datastructure_id IF EXISTS;", schema_text)
        self.assertIn("CREATE CONSTRAINT datastructure_uid", schema_text)
        self.assertIn("FOR (n:DataStructure) REQUIRE n.uid IS UNIQUE;", schema_text)
        self.assertNotIn(
            "CREATE CONSTRAINT datastructure_id IF NOT EXISTS FOR (n:DataStructure) REQUIRE n.id IS UNIQUE;",
            schema_text,
        )


if __name__ == "__main__":
    unittest.main()
