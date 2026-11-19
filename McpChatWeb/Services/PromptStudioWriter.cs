using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public static class PromptStudioWriter
{
	public sealed record SaveResult(bool Saved, string? Reason);

	// Refreshes only the Prompt Studio notes block; the placeholder guard still has the final word.
	public static SaveResult SaveNotes(string promptFile, string notes)
	{
		try
		{
			var before = File.ReadAllText(promptFile);
			var after = PromptStudioNotes.Apply(before, notes);
			var dropped = PromptPlaceholders.Dropped(before, after);
			if (dropped.Count > 0)
				return new SaveResult(false, PromptPlaceholders.DescribeLoss(Path.GetFileNameWithoutExtension(promptFile), dropped));

			File.WriteAllText(promptFile, after);
			return new SaveResult(true, null);
		}
		catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or InvalidOperationException)
		{
			return new SaveResult(false, ex.Message);
		}
	}
}
