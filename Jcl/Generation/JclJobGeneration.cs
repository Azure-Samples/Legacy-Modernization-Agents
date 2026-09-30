using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Jcl.Generation;

// The conversion's hook: jobs are written into the run folder before it is scaffolded and compiled.
public static class JclJobGeneration
{
    public static int Run(JclJobsSettings settings, string cobolSourceFolder, string outputFolder, TargetLanguage language, ILogger logger)
    {
        if (!settings.Enabled) return 0;
        var source = string.IsNullOrWhiteSpace(settings.SourceFolder) ? cobolSourceFolder : settings.SourceFolder;
        try
        {
            var written = JclJobWriter.WriteTo(source, outputFolder, language == TargetLanguage.CSharp ? "C#" : "Java");
            if (written > 0)
                logger.LogInformation("Generated {Jobs} job(s) from the JCL in {Source}; see {Manifest}",
                    written, source, Path.Join(outputFolder, JclJobWriter.ManifestFile));
            return written;
        }
        catch (Exception ex) when (ex is not OperationCanceledException)
        {
            // Jobs are additive; the converted programs stand without them.
            logger.LogWarning("Could not generate jobs from JCL: {Message}", ex.Message);
            return 0;
        }
    }
}
