import sys
import types
import unittest
from pathlib import Path
from unittest.mock import patch


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

from populator import ingest_rekt_data_structures, ingest_rekt_outputs
from source_paths import scoped_graph_id


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
