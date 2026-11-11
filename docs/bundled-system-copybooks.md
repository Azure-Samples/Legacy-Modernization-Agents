# Bundled system copybooks

**Last updated**: 2026-10-27

`tools/system-copybooks/` supplies parser inputs when no accepted definition of
the same name exists in the source tree. Supplied definitions take precedence.
Unverified generated candidates and placeholders do not suppress a bundled
definition. Original business sources and rejected candidates are not modified.
The existing preprocessing/staging provenance chain records the selected file
and verifies its bytes; strict dependency evidence remains required.

## SQLDA

`tools/system-copybooks/SQLDA.cpy` is the unmodified SQLDA parser definition from
the Eclipse COBOL language server, copyright Broadcom, licensed under
[EPL-2.0](../tools/system-copybooks/LICENSE.EPL-2.0). Its copyright and license
notice are retained in the file, with the full license supplied beside it.

- Upstream: <https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol>
- Revision: `7b7c08023c1a49ec95a9d87406944f3e1236d28b`
- Path: `server/engine/src/main/resources/implicitCopybooks/SQLDA.cpy`
- SHA256: `1b7cca4a8cf2428a1adfda82c898a4b18bbdf012047248398bba85571e7cd942`

Staging an explicit file lets the native exporter hash the same definition used
by analysis, instead of treating an `implicit://` parser URI as a filesystem
path. This is a declared parser-supplied definition, not an assertion that it
matches a customer's installed Db2 version, compiler ABI or encoding. For an
assessment of a real estate, supply its actual SDK copybooks.

## Bank-of-Z platform inputs

Bank-of-Z is a demo application, not a self-contained replacement for z/OS
middleware. Its build configuration adds `SDFHCOB` to the COBOL `SYSLIB` search
path for CICS programs:
<https://github.com/IBM/Bank-of-Z/blob/main/.setup/build/languages/Cobol.yaml>.
Its [prerequisites](https://ibm.github.io/Bank-of-Z/docs/installation-and-setup/prerequisites)
include CICS and Db2.

The local upstream test checkout contains DFHAID and DFHBMSCA files bearing IBM
restricted-material notices. They are not bundled or copied into this project.
Obtain appropriately licensed, version-matching CICS definitions before using
them as conversion evidence. The incomplete files under `copy-generated/` are
not approved substitutes, and demo status does not waive these requirements.

For modernization that replaces terminal input with named target events, an
[explicit platform-replacement contract](platform-replacement-contracts.md)
can account for supported DFHAID comparisons without supplying vendor bytes.
This does not resolve aggregate/layout uses or DFHBMSCA.
