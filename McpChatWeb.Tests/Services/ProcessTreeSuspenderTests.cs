using System;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Services;

public class ProcessTreeSuspenderTests
{
    [Fact]
    public void Suspend_InvalidProcessReportsFailureOnWindows()
    {
        if (!OperatingSystem.IsWindows()) return;
        Assert.Throws<System.ComponentModel.Win32Exception>(
            () => ProcessTreeSuspender.Suspend(int.MaxValue, includeRoot: true));
    }

    [Fact]
    public void Descendants_ListsRootThenChildrenBreadthFirst()
    {
        var map = new[] { (10, 1), (11, 10), (12, 10), (13, 11), (99, 1), (10, 10) };

        Assert.Equal(new[] { 10, 11, 12, 13 }, ProcessTreeSuspender.Descendants(10, map));
    }

    [Fact]
    public void Descendants_StopsOnCycles()
    {
        var map = new[] { (2, 1), (1, 2) };

        Assert.Equal(new[] { 1, 2 }, ProcessTreeSuspender.Descendants(1, map));
    }

    [Fact]
    public void ParsePsOutput_ReadsPidAndParent()
    {
        var parsed = ProcessTreeSuspender.ParsePsOutput("  101   1\n  202 101\nbogus line\n").ToList();

        Assert.Equal(new[] { (101, 1), (202, 101) }, parsed);
    }

    [Fact]
    public void SuspendAndResume_StopEverythingBelowTheShellWrapper()
    {
        if (OperatingSystem.IsWindows()) return;

        // Same layout as a portal run: sh wrapper -> worker -> grandchild.
        using var root = Process.Start(new ProcessStartInfo("/bin/sh",
            new[] { "-c", "\"$@\"; exit $?", "migration-run", "sh", "-c", "sleep 30 & wait" })
        {
            UseShellExecute = false
        })!;
        try
        {
            var tree = new System.Collections.Generic.List<int>();
            for (var i = 0; i < 50 && tree.Count < 3; i++)
            {
                tree = ProcessTreeSuspender.ProcessTree(root.Id);
                if (tree.Count < 3) Thread.Sleep(100);
            }
            Assert.Equal(3, tree.Count);

            ProcessTreeSuspender.Suspend(root.Id, includeRoot: false);
            // State() starts a process, which would hang if the test's own child were stopped.
            Assert.StartsWith("T", State(tree[1]));
            Assert.StartsWith("T", State(tree[2]));
            Assert.DoesNotContain("T", State(root.Id));

            ProcessTreeSuspender.Resume(root.Id, includeRoot: false);
            Assert.DoesNotContain("T", State(tree[1]));
            Assert.DoesNotContain("T", State(tree[2]));
        }
        finally
        {
            root.Kill(entireProcessTree: true);
        }
    }

    [Fact]
    public void SuspendAndResume_SuspendTheWholeTreeOnWindows()
    {
        if (!OperatingSystem.IsWindows()) return;

        using var root = Process.Start(new ProcessStartInfo("cmd.exe", new[] { "/c", "ping -n 30 127.0.0.1 >nul" })
        {
            UseShellExecute = false,
            CreateNoWindow = true
        })!;
        try
        {
            var tree = new System.Collections.Generic.List<int>();
            for (var i = 0; i < 50 && tree.Count < 2; i++)
            {
                tree = ProcessTreeSuspender.ProcessTree(root.Id);
                if (tree.Count < 2) Thread.Sleep(100);
            }
            Assert.True(tree.Count >= 2, "cmd.exe should have started ping.exe");

            ProcessTreeSuspender.Suspend(root.Id, includeRoot: true);
            foreach (var pid in tree)
                Assert.True(SpinWait.SpinUntil(() => AllThreadsSuspended(pid), TimeSpan.FromSeconds(5)),
                    $"process {pid} should be suspended within 5 seconds");

            ProcessTreeSuspender.Resume(root.Id, includeRoot: true);
            foreach (var pid in tree)
                Assert.True(SpinWait.SpinUntil(() => !AllThreadsSuspended(pid), TimeSpan.FromSeconds(5)),
                    $"process {pid} should be running within 5 seconds");
        }
        finally
        {
            root.Kill(entireProcessTree: true);
        }
    }

    private static bool AllThreadsSuspended(int pid)
    {
        using var process = Process.GetProcessById(pid);
        return process.Threads.Cast<ProcessThread>().All(t =>
            t.ThreadState == System.Diagnostics.ThreadState.Wait && t.WaitReason == ThreadWaitReason.Suspended);
    }

    private static string State(int pid)
    {
        var psi = new ProcessStartInfo("ps", new[] { "-o", "stat=", "-p", pid.ToString() })
        {
            RedirectStandardOutput = true,
            UseShellExecute = false
        };
        using var ps = Process.Start(psi)!;
        var output = ps.StandardOutput.ReadToEnd().Trim();
        ps.WaitForExit();
        return output;
    }
}
