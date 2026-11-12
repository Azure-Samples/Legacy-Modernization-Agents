# Ask the Report

**Last updated**: 2026-10-07

The portal chat can answer questions from a reverse-engineering (RE) report instead of the migration database. This page explains how that mode picks report content, how saved chats work, and the settings that control both.

## Using it

1. Run reverse engineering so that `output/` contains a report (for example `output/reverse-engineering-details.md`).
2. In the portal, open **Chat Context** in the left panel and turn on **Chat with RE Report**.
3. Pick a report. **Latest report** is the default: the newest reverse-engineering report, resolved each time you ask, so a new RE run is picked up without reselecting.
4. Ask a question, for example "What does PAYRB005 check before posting?"

Turn the toggle off to answer from the migration database again. The transcript shows a notice each time the source changes.

## How answers are scoped

Report mode answers **only the question asked**:

- It returns before any run, file or database context is gathered. Questions about a program are not mixed with run statistics, and the answer is not a summary of the latest run.
- The model is told not to summarise the report, the run or the estate unless the question asks for a summary.
- It answers only from the report. If the report doesn't contain the answer, it says so and names the closest sections.

Report size decides what is sent:

| Report size | What is sent |
|---|---|
| Up to `WholeReportMaxChars` | The whole report. |
| Larger | An outline of every heading, plus the sections that best match the question, up to `SectionBudgetChars`. |

Sections are split on Markdown headings (`#` to `####`); hashes inside code fences are ignored. When picking sections:

- Rare words in the question count more than common ones.
- A match in a heading counts more than one in body text, so asking about `PAYRB005` lands on the `PAYRB005` section.
- For a follow-up such as "when does it run?", the previous question's terms are also used, at half weight.

The database chat gets the same answer-only instruction. Its database context is labelled as reference material.

The MCP server returns the model's answer on its own. It used to wrap every answer in the run status, metrics, insights, a banner and the top dependencies. Those now appear only as a labelled fallback when no chat model is configured or the model call fails. Report questions are sent with `context: "report"`, so the server adds no run context and works before any migration run exists.

## Saved chats

Every question and answer is saved in the browser (`localStorage`) and listed under **Saved Chats**, grouped by Today, Yesterday, Previous 7 days and Older.

- **Resume:** click a saved chat to continue it. Recent turns are sent with the next question so follow-ups keep their subject.
- **Search:** searches titles and message text.
- **Delete:** removes a single chat.
- **Scope:** each answer records whether it came from a report (and which one) or from the database.
- **Interrupted answers:** an answer interrupted by a reload is marked as not received.

Saved chats stay in the browser that created them. Nothing is written to the server.

## Configuration

`Config/appsettings.json`, section `PortalChat`:

| Key | Default | Meaning |
|---|---|---|
| `WholeReportMaxChars` | 60000 | Reports up to this size are sent whole. |
| `SectionBudgetChars` | 40000 | Room for the sections picked from a larger report. |
| `OutlineMaxChars` | 4000 | Size cap for the heading outline sent with a cut report. |
| `MaxHistoryMessages` | 8 | Earlier turns sent with each question. |
| `MaxHistoryMessageChars` | 1500 | Each earlier turn is cut to this length. |
| `MaxSavedConversations` | 100 | Saved chats kept in the browser; the oldest are dropped first. |
| `MaxAnswerTokens` | 4000 | Output-token cap for each chat answer, in both report and database mode. |

## API

| Endpoint | Purpose |
|---|---|
| `POST /api/chat` | `{ prompt, reportContext, history }`. `reportContext` is a report path relative to the repository, or `latest`. `history` is a list of earlier `{ role, content }` turns. |
| `GET /api/reports/available` | Reports found under `output/`, newest first. |
| `GET /api/chat/settings` | `MaxHistoryMessages` and `MaxSavedConversations` for the browser. |

Report paths are resolved inside the repository root; paths outside it are refused.
