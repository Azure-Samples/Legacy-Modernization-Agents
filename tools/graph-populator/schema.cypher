// Neo4j schema for the unified Cobol-REKT and migration graph.
// Applied once by the Python graph-populator on first run.

// Uniqueness constraints
CREATE CONSTRAINT run_id          IF NOT EXISTS FOR (n:Run)           REQUIRE n.id IS UNIQUE;
CREATE CONSTRAINT cobolfile_uid   IF NOT EXISTS FOR (n:CobolFile)     REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT astnode_id      IF NOT EXISTS FOR (n:ASTNode)       REQUIRE n.id IS UNIQUE;
CREATE CONSTRAINT cfgnode_id      IF NOT EXISTS FOR (n:CFGNode)       REQUIRE n.id IS UNIQUE;
CREATE CONSTRAINT datastructure_id IF NOT EXISTS FOR (n:DataStructure) REQUIRE n.id IS UNIQUE;
CREATE CONSTRAINT chunk_uid       IF NOT EXISTS FOR (n:Chunk)         REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT signature_uid   IF NOT EXISTS FOR (n:Signature)     REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT typemapping_uid IF NOT EXISTS FOR (n:TypeMapping)   REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT businesslogic_uid IF NOT EXISTS FOR (n:BusinessLogic) REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT businessrule_uid IF NOT EXISTS FOR (n:BusinessRule) REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT dataentity_uid  IF NOT EXISTS FOR (n:DataEntity)    REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT service_uid     IF NOT EXISTS FOR (n:Service)       REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT capability_uid  IF NOT EXISTS FOR (n:Capability)    REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT sourceblock_uid IF NOT EXISTS FOR (n:SourceBlock)   REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT flowchart_uid   IF NOT EXISTS FOR (n:Flowchart)     REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT comment_uid     IF NOT EXISTS FOR (n:Comment)       REQUIRE n.uid IS UNIQUE;
CREATE CONSTRAINT metrics_uid     IF NOT EXISTS FOR (n:Metrics)       REQUIRE n.uid IS UNIQUE;

// Composite indexes
CREATE INDEX astnode_program_type IF NOT EXISTS FOR (n:ASTNode)       ON (n.program, n.nodeType);
CREATE INDEX astnode_program_line IF NOT EXISTS FOR (n:ASTNode)       ON (n.program, n.startLine);
CREATE INDEX cfgnode_program      IF NOT EXISTS FOR (n:CFGNode)       ON (n.program);
CREATE INDEX cobolfile_run        IF NOT EXISTS FOR (n:CobolFile)     ON (n.runId);
CREATE INDEX datastructure_prog   IF NOT EXISTS FOR (n:DataStructure) ON (n.program);
CREATE INDEX chunk_run_file       IF NOT EXISTS FOR (n:Chunk)         ON (n.runId, n.sourceFile);
CREATE INDEX signature_run_file   IF NOT EXISTS FOR (n:Signature)     ON (n.runId, n.sourceFile);
CREATE INDEX typemapping_run      IF NOT EXISTS FOR (n:TypeMapping)   ON (n.runId, n.sourceFile);
CREATE INDEX businesslogic_run    IF NOT EXISTS FOR (n:BusinessLogic) ON (n.runId);
CREATE INDEX sourceblock_file     IF NOT EXISTS FOR (n:SourceBlock)   ON (n.program, n.blockIndex);

// Full-text indexes
CREATE FULLTEXT INDEX cobol_search IF NOT EXISTS
  FOR (n:CobolFile|ASTNode|DataStructure)
  ON EACH [n.content, n.name, n.fileName, n.originalText];
