using System.Diagnostics;
using System.Runtime.InteropServices;

namespace McpChatWeb.Services;

/// <summary>
/// Pauses and resumes a process together with every process it started. Runs are launched
/// through <c>dotnet run</c>, which hosts the migration in a child process, so signalling only
/// the root would leave the actual work running.
/// On Unix a .NET process whose child is stopped busy-loops at full CPU and blocks Process.Start
/// until the child continues, so the portal launches runs under a non-.NET <c>sh</c> wrapper and
/// leaves that root running (includeRoot false) while everything below it stops.
/// </summary>
public static class ProcessTreeSuspender
{
    public static void Suspend(int rootPid, bool includeRoot)
    {
        // Parents first, so none can start new children while the rest are being stopped.
        foreach (var pid in ProcessTree(rootPid).Skip(includeRoot ? 0 : 1))
            Signal(pid, suspend: true);
    }

    public static void Resume(int rootPid, bool includeRoot)
    {
        foreach (var pid in ProcessTree(rootPid).Skip(includeRoot ? 0 : 1).Reverse())
            Signal(pid, suspend: false);
    }

    /// <summary>The root followed by its descendants, parents before children.</summary>
    public static List<int> ProcessTree(int rootPid) =>
        Descendants(rootPid, OperatingSystem.IsWindows() ? WindowsParentMap() : UnixParentMap());

    public static List<int> Descendants(int rootPid, IEnumerable<(int Pid, int ParentPid)> parentMap)
    {
        var children = parentMap
            .Where(p => p.Pid != p.ParentPid)
            .GroupBy(p => p.ParentPid)
            .ToDictionary(g => g.Key, g => g.Select(p => p.Pid).ToList());

        var ordered = new List<int> { rootPid };
        var seen = new HashSet<int> { rootPid };
        for (var i = 0; i < ordered.Count; i++)
        {
            if (!children.TryGetValue(ordered[i], out var kids)) continue;
            foreach (var kid in kids)
                if (seen.Add(kid)) ordered.Add(kid);
        }
        return ordered;
    }

    public static IEnumerable<(int Pid, int ParentPid)> ParsePsOutput(string output)
    {
        foreach (var line in output.Split('\n', StringSplitOptions.RemoveEmptyEntries))
        {
            var parts = line.Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length >= 2 && int.TryParse(parts[0], out var pid) && int.TryParse(parts[1], out var ppid))
                yield return (pid, ppid);
        }
    }

    private static void Signal(int pid, bool suspend)
    {
        if (OperatingSystem.IsWindows())
        {
            WindowsSignal(pid, suspend);
            return;
        }

        var psi = new ProcessStartInfo { FileName = "kill", UseShellExecute = false, CreateNoWindow = true };
        psi.ArgumentList.Add(suspend ? "-STOP" : "-CONT");
        psi.ArgumentList.Add(pid.ToString());
        using var kill = Process.Start(psi);
        kill?.WaitForExit(3000);
    }

    private static List<(int, int)> UnixParentMap()
    {
        var psi = new ProcessStartInfo
        {
            FileName = "ps",
            RedirectStandardOutput = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };
        foreach (var arg in new[] { "-A", "-o", "pid=", "-o", "ppid=" })
            psi.ArgumentList.Add(arg);

        using var ps = Process.Start(psi);
        if (ps == null) return new();
        var output = ps.StandardOutput.ReadToEnd();
        ps.WaitForExit(3000);
        return ParsePsOutput(output).ToList();
    }

    private static List<(int, int)> WindowsParentMap()
    {
        var map = new List<(int, int)>();
        var snapshot = CreateToolhelp32Snapshot(SnapProcess, 0);
        if (snapshot == InvalidHandle) return map;
        try
        {
            var entry = new ProcessEntry32 { dwSize = (uint)Marshal.SizeOf<ProcessEntry32>() };
            for (var ok = Process32FirstW(snapshot, ref entry); ok; ok = Process32NextW(snapshot, ref entry))
                map.Add(((int)entry.th32ProcessID, (int)entry.th32ParentProcessID));
        }
        finally
        {
            CloseHandle(snapshot);
        }
        return map;
    }

    private static void WindowsSignal(int pid, bool suspend)
    {
        var handle = OpenProcess(ProcessSuspendResume, false, (uint)pid);
        if (handle == IntPtr.Zero) return;
        try
        {
            if (suspend) NtSuspendProcess(handle);
            else NtResumeProcess(handle);
        }
        finally
        {
            CloseHandle(handle);
        }
    }

    private const uint SnapProcess = 0x00000002;
    private const uint ProcessSuspendResume = 0x0800;
    private static readonly IntPtr InvalidHandle = new(-1);

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    private struct ProcessEntry32
    {
        public uint dwSize;
        public uint cntUsage;
        public uint th32ProcessID;
        public IntPtr th32DefaultHeapID;
        public uint th32ModuleID;
        public uint cntThreads;
        public uint th32ParentProcessID;
        public int pcPriClassBase;
        public uint dwFlags;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 260)]
        public string szExeFile;
    }

    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern IntPtr CreateToolhelp32Snapshot(uint flags, uint processId);

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    private static extern bool Process32FirstW(IntPtr snapshot, ref ProcessEntry32 entry);

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    private static extern bool Process32NextW(IntPtr snapshot, ref ProcessEntry32 entry);

    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern IntPtr OpenProcess(uint access, bool inherit, uint processId);

    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool CloseHandle(IntPtr handle);

    [DllImport("ntdll.dll")]
    private static extern int NtSuspendProcess(IntPtr processHandle);

    [DllImport("ntdll.dll")]
    private static extern int NtResumeProcess(IntPtr processHandle);
}
