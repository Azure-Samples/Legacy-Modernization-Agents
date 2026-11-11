#!/usr/bin/env python3
import importlib.util
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "tools" / "bms_symbolic_map.py"
sys.path.insert(0, str(SCRIPT_PATH.parent))

_spec = importlib.util.spec_from_file_location("bms_symbolic_map", SCRIPT_PATH)
bms = importlib.util.module_from_spec(_spec)
sys.modules["bms_symbolic_map"] = bms
_spec.loader.exec_module(bms)


def card(text, continued=False):
    """One assembler card: columns 1-71, then a continuation mark in column 72."""
    assert len(text) <= 71, text
    return text.ljust(71) + ("*" if continued else "")


MAPSET = "\n".join([
    "* comment line",
    card("ORDSET   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,TIOAPFX=YES,", True),
    card("               DSATTS=(COLOR,HILIGHT)"),
    card("ORDMAP   DFHMDI SIZE=(24,80)"),
    card("         DFHMDF POS=(1,1),LENGTH=7,INITIAL='ORDMAP '"),
    card("TITLE    DFHMDF POS=(1,10),LENGTH=30,INITIAL='Order entry, page 1 - A l", True),
    card("               ong title'"),
    card("ORDNO    DFHMDF POS=(3,10),LENGTH=8,ATTRB=(NORM,NUM)"),
    card("RATE     DFHMDF POS=(4,10),LENGTH=7,PICOUT='9999.99'"),
    card("LINE     DFHMDF POS=(6,1),LENGTH=40,OCCURS=3"),
    card("         DFHMSD TYPE=FINAL"),
    "         END",
])


class ParseTests(unittest.TestCase):
    def test_continuation_joins_quoted_initial_without_dropping_operands(self):
        stmts = [bms.parse_statement(l) for l in bms.logical_lines(MAPSET)]
        title = next(s for s in stmts if s.label == "TITLE")
        self.assertEqual(title.operands["INITIAL"], "Order entry, page 1 - A long title")
        self.assertEqual(title.operands["LENGTH"], "30")

        mapset = next(s for s in stmts if s.macro == "DFHMSD")
        self.assertEqual(mapset.operands["DSATTS"], ("COLOR", "HILIGHT"))

    def test_unnamed_fields_are_not_in_the_map(self):
        (mapset,) = bms.parse_bms(MAPSET)
        self.assertEqual([f.name for f in mapset.maps[0].fields], ["TITLE", "ORDNO", "RATE", "LINE"])

    def test_extatt_yes_implies_default_attributes(self):
        text = "\n".join([
            card("S        DFHMSD TYPE=MAP,MODE=OUT,LANG=COBOL,EXTATT=YES"),
            card("M        DFHMDI SIZE=(24,80)"),
            card("F        DFHMDF POS=(1,1),LENGTH=2"),
        ])
        (mapset,) = bms.parse_bms(text)
        self.assertEqual(mapset.maps[0].attributes, ("COLOR", "PS", "HILIGHT", "VALIDN"))

    def test_screen_metadata_keeps_unnamed_labels_positions_and_input_constraints(self):
        screen = bms.parse_bms(MAPSET)[0].maps[0]
        self.assertEqual((24, 80), screen.size)
        self.assertEqual("", screen.screen_fields[0].name)
        self.assertEqual("ORDMAP ", screen.screen_fields[0].initial)
        self.assertEqual((1, 1), screen.screen_fields[0].position)
        order_number = next(field for field in screen.screen_fields if field.name == "ORDNO")
        self.assertEqual(("NORM", "NUM"), order_number.attributes)
        self.assertEqual(8, order_number.length)

    def test_invalid_field_lengths_are_not_replaced_with_invented_single_bytes(self):
        with self.assertRaises(ValueError):
            bms.parse_bms(MAPSET.replace("LENGTH=8", "LENGTH=UNKNOWN"))

    def test_missing_lengths_and_invalid_coordinates_are_explicitly_unsupported(self):
        with self.assertRaises(ValueError):
            bms.parse_bms(MAPSET.replace("LENGTH=8,", ""))
        with self.assertRaises(ValueError):
            bms.parse_bms(MAPSET.replace("POS=(3,10)", "POS=(0,10)"))

    def test_zero_length_unnamed_attribute_boundary_is_retained_without_fake_data(self):
        screen = bms.parse_bms(MAPSET.replace("LENGTH=7,INITIAL='ORDMAP '", "LENGTH=0"))[0].maps[0]
        self.assertEqual(0, screen.screen_fields[0].length)
        self.assertEqual("", screen.screen_fields[0].name)
        self.assertNotIn(screen.screen_fields[0], screen.fields)

class RenderTests(unittest.TestCase):
    def setUp(self):
        self.copybook = bms.render_copybook(bms.parse_bms(MAPSET), "ORDSET.bms")

    def test_input_and_output_structures_share_storage(self):
        self.assertIn("       01  ORDMAPI.", self.copybook)
        self.assertIn("       01  ORDMAPO REDEFINES ORDMAPI.", self.copybook)

    def test_map_storage_overlay_and_auto_prefix_follow_the_original_mapset(self):
        extra = "\n".join([card("SECOND   DFHMDI SIZE=(24,80)"), card("OTHER    DFHMDF POS=(1,1),LENGTH=4")])
        text = MAPSET.replace(card("         DFHMSD TYPE=FINAL"), extra + "\n" + card("         DFHMSD TYPE=FINAL"))
        rendered = bms.render_copybook(bms.parse_bms(text), "ORDSET.bms")
        self.assertIn("01  SECONDI REDEFINES ORDMAPI.", rendered)
        automatic = text.replace("TIOAPFX=YES", "STORAGE=AUTO")
        rendered = bms.render_copybook(bms.parse_bms(automatic), "ORDSET.bms")
        self.assertIn("01  SECONDI.", rendered)
        self.assertNotIn("01  SECONDI REDEFINES", rendered)
        self.assertEqual(4, rendered.count("02  FILLER PIC X(12)."))

    def test_field_suffixes_follow_cics_layout(self):
        for line in ("ORDNOL    COMP  PIC  S9(4).", "ORDNOF    PICTURE X.",
                     "ORDNOA    PICTURE X.", "ORDNOI  PIC X(8).",
                     "ORDNOC    PICTURE X.", "ORDNOH    PICTURE X.", "ORDNOO  PIC X(8)."):
            self.assertIn(line, self.copybook)
        self.assertNotIn("ORDNOP", self.copybook, "PS is not in DSATTS")

    def test_input_and_output_entries_are_the_same_length(self):
        # L(2) + F(1) + attributes(2) + data == 3 + attributes(2) + data, so I and O line up.
        self.assertIn("02  FILLER   PICTURE X(2).", self.copybook)
        self.assertEqual(self.copybook.count("02  FILLER PIC X(12)."), 2)

    def test_picout_and_occurs(self):
        self.assertIn("RATEO  PIC 9999.99.", self.copybook)
        self.assertIn("RATEI  PIC X(7).", self.copybook)
        self.assertIn("02  DFHMS1 OCCURS 3 TIMES.", self.copybook)
        self.assertIn("03  LINEI  PIC X(40).", self.copybook)
        self.assertIn("03  LINEO  PIC X(40).", self.copybook)

    def test_fits_fixed_format_columns(self):
        for line in self.copybook.splitlines():
            self.assertLessEqual(len(line), 72, line)


class GenerateTests(unittest.TestCase):
    def setUp(self):
        self.work = Path(tempfile.mkdtemp(prefix="bms-map-"))
        self.source = self.work / "source"
        self.out = self.source / ".preprocessed"
        (self.source / "bms").mkdir(parents=True)
        (self.source / "bms" / "ordset.bms").write_text(MAPSET)

    def tearDown(self):
        shutil.rmtree(self.work, ignore_errors=True)

    def run_cli(self):
        return subprocess.run([sys.executable, str(SCRIPT_PATH), str(self.source), str(self.out)],
                              capture_output=True, text=True, check=True)

    def test_copybook_is_named_after_the_source_file(self):
        result = self.run_cli()
        self.assertTrue((self.out / "ORDSET.cpy").exists())
        self.assertIn("ORDSET", result.stdout)

    def test_manifest_binds_screen_fields_to_the_actual_bms_source(self):
        self.run_cli()
        manifest = json.loads((self.out / "bms-screen-definitions.json").read_text())
        definition = manifest["definitions"][0]
        self.assertEqual("bms/ordset.bms", definition["path"])
        self.assertEqual("ORDSET", definition["resourceName"])
        self.assertEqual(bms.screen_definition(self.source / "bms/ordset.bms", self.source)["sha256"],
                         definition["sha256"])
        screen = definition["mapsets"][0]["maps"][0]
        self.assertEqual([24, 80], screen["size"])
        self.assertEqual("ORDMAP ", screen["screen_fields"][0]["initial"])

    def test_supported_layout_records_exact_offsets_and_rejects_changed_copybook(self):
        self.run_cli()
        copybook = self.out / "ORDSET.cpy"
        proof = json.loads(Path(str(copybook) + ".bms-layout.json").read_text())
        self.assertEqual("complete", proof["status"])
        self.assertEqual(207, proof["maps"][0]["byteLength"])
        order, = [field for field in proof["maps"][0]["fields"] if field["name"] == "ORDNO"]
        self.assertEqual((47, 52, 13), (order["offset"], order["dataOffset"], order["stride"]))
        self.assertEqual(proof, bms.complete_layout_evidence(self.source / "bms/ordset.bms", copybook))
        copybook.write_text(copybook.read_text().replace("ORDNOI  PIC X(8)", "ORDNOI  PIC X(9)"))
        with self.assertRaisesRegex(ValueError, "differs"):
            bms.complete_layout_evidence(self.source / "bms/ordset.bms", copybook)

    def test_unsupported_or_malformed_layouts_remain_explicitly_partial(self):
        path = self.source / "bms/ordset.bms"
        for changed in (MAPSET.replace("LENGTH=8", "LENGTH=8,GRPNAME=GROUP"),
                        MAPSET.replace("PICOUT='9999.99'", "PICOUT='999.99'"),
                        MAPSET.replace("INITIAL='ORDMAP '", "INITIAL='ORDMAP "),
                        MAPSET.rsplit("\n", 1)[0] + "\n" + card("         END", True)):
            with self.subTest(changed=changed):
                path.write_text(changed)
                result = self.run_cli()
                proof = json.loads((self.out / "ORDSET.cpy.bms-layout.json").read_text())
                self.assertEqual("partial", proof["status"])
                self.assertTrue(proof["diagnostics"])
                self.assertIn("remains partial", result.stderr)

    def test_real_copybook_in_source_wins(self):
        (self.source / "cpy").mkdir()
        (self.source / "cpy" / "ORDSET.cpy").write_text("       01  REAL-MAP PIC X.\n")
        self.run_cli()
        self.assertFalse((self.out / "ORDSET.cpy").exists())

    def test_replaces_stub_but_not_other_content(self):
        self.out.mkdir()
        (self.out / "ORDSET.cpy").write_text(f"      *> {bms.STUB_MARKER} for ORDSET\n")
        self.run_cli()
        self.assertIn(bms.GENERATED_MARKER, (self.out / "ORDSET.cpy").read_text())

        (self.out / "ORDSET.cpy").write_text("       01  HAND-WRITTEN PIC X.\n")
        self.run_cli()
        self.assertIn("HAND-WRITTEN", (self.out / "ORDSET.cpy").read_text())

    def test_wrong_arguments_print_usage(self):
        result = subprocess.run([sys.executable, str(SCRIPT_PATH)], capture_output=True, text=True)
        self.assertEqual(result.returncode, 2)
        self.assertIn("Usage: bms_symbolic_map.py <source-dir> <output-dir>", result.stderr)

    def test_non_cobol_mapsets_are_skipped(self):
        (self.source / "bms" / "ordset.bms").write_text(MAPSET.replace("LANG=COBOL", "LANG=ASM  "))
        self.run_cli()
        self.assertFalse((self.out / "ORDSET.cpy").exists())

    def test_unproven_generated_copy_does_not_override_current_grounded_bms(self):
        generated = self.source / "copy-generated"
        generated.mkdir()
        (generated / "ORDSET.cpy").write_text("       01 UNPROVEN-SUBSET PIC X.\n")
        self.run_cli()
        copybook = self.out / "ORDSET.cpy"
        self.assertIn("ORDNOI", copybook.read_text())
        record = json.loads(Path(str(copybook) + ".preprocess.json").read_text())
        self.assertEqual("bms-symbolic-map", record["producer"])
        self.assertEqual(str((self.source / "bms/ordset.bms").resolve()), record["originalSourcePath"])


if __name__ == "__main__":
    unittest.main()
