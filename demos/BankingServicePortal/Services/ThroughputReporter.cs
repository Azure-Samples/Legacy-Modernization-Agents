// Reports how much work a batch run is getting through.
//
// Modelled on the batch display module in the estate, which printed a line showing how many
// logical units a program was handling per minute. The COBOL wrote it to the job log; here the
// same reading is returned as data and the log line is preserved alongside it.

namespace Modernized.Banking.Portal.Services;

using System.Globalization;
using Modernized.Banking.Portal.Domain;

public sealed class ThroughputReporter : IThroughputReporter
{
    public ThroughputResult Measure(ThroughputRequest request)
    {
        var label = string.IsNullOrWhiteSpace(request.Label) ? "BATCH" : request.Label.Trim();
        var units = Math.Max(0, request.UnitsProcessed);

        // A run shorter than the clock's resolution would divide to infinity rather than to a
        // very large rate, so it is reported as zero and the caller can see why from the inputs.
        var perMinute = request.ElapsedSeconds <= 0
            ? 0d
            : Math.Round(units / request.ElapsedSeconds * 60d, 1, MidpointRounding.AwayFromZero);

        var line = string.Format(
            CultureInfo.InvariantCulture,
            "{0,-20} {1,10:N0} units {2,10:N1} sec {3,12:N1} units/min",
            label, units, request.ElapsedSeconds, perMinute);

        return new ThroughputResult(label, units, request.ElapsedSeconds, perMinute, line);
    }
}
