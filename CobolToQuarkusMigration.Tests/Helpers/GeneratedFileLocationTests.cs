using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// A generated file whose declaration disagrees with its folder does not build, and nothing in the
// pipeline notices: the conversion reports success and the estate looks converted.
public sealed class GeneratedFileLocationTests : IDisposable
{
    private readonly string _root = Path.Join(
        Path.GetTempPath(), $"gen-loc-{Guid.NewGuid():N}");

    private FileHelper Helper() => new(NullLogger<FileHelper>.Instance);

    [Fact]
    public async Task AJavaFileIsFiledUnderThePackageItDeclares()
    {
        // Observed on a real estate: every file landed in com/example/generated while declaring
        // something else, so none of the output compiled as a project.
        var file = new JavaFile
        {
            FileName = "Kyghr002.java",
            NamespaceName = "com.example.generated",
            Content = "package com.example.cobol.kyghr002;\n\npublic class Kyghr002 { }\n",
        };

        var saved = await Helper().SaveJavaFileAsync(file, _root);

        saved.Replace('\\', '/').Should().Contain("com/example/cobol/kyghr002/");
    }

    [Fact]
    public async Task ACSharpFileIsFiledUnderTheNamespaceItDeclares()
    {
        // C# does not require the two to agree, but a namespace that disagrees with its folder is
        // still the kind of thing a reviewer stops on, and the extraction has to understand the
        // C# forms at all — it previously only recognised Java's `package`.
        var file = new CodeFile
        {
            FileName = "Kyghr002.cs",
            NamespaceName = "",
            Content = "namespace Contoso.Billing.Kyghr002;\n\npublic class Kyghr002 { }\n",
        };

        var saved = await Helper().SaveCodeFileAsync(file, _root, ".cs");

        saved.Replace('\\', '/').Should().Contain("Contoso/Billing/Kyghr002/");
    }

    [Fact]
    public async Task ABracedCSharpNamespaceIsAlsoUnderstood()
    {
        var file = new CodeFile
        {
            FileName = "Legacy.cs",
            NamespaceName = "",
            Content = "namespace Contoso.Legacy\n{\n    public class Legacy { }\n}\n",
        };

        var saved = await Helper().SaveCodeFileAsync(file, _root, ".cs");

        saved.Replace('\\', '/').Should().Contain("Contoso/Legacy/");
    }

    [Fact]
    public async Task AFileDeclaringNothingKeepsTheNamespaceItWasGiven()
    {
        var file = new JavaFile
        {
            FileName = "Fragment.java",
            NamespaceName = "com.example.billing",
            Content = "public class Fragment { }\n",
        };

        var saved = await Helper().SaveJavaFileAsync(file, _root);

        saved.Replace('\\', '/').Should().Contain("com/example/billing/");
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
