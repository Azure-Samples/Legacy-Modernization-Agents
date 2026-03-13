using Xunit;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using Moq;
using CobolToQuarkusMigration.Agents.Infrastructure;
using GitHub.Copilot.SDK;

using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

public class CopilotChatClientTests
{
    private const string TestModel = "gpt-4o";

    private static CopilotChatClient CreateClient(
        string model = TestModel,
        CopilotClientOptions? options = null,
        ILogger? logger = null)
    {
        return new CopilotChatClient(model, options, logger);
    }

    #region Constructor

    [Fact]
    public void Constructor_NullModel_ThrowsArgumentNullException()
    {
        var act = () => new CopilotChatClient(null!);

        act.Should().Throw<ArgumentNullException>()
            .And.ParamName.Should().Be("model");
    }

    [Fact]
    public void Constructor_ValidModel_DoesNotThrow()
    {
        using var client = CreateClient();
        client.Should().NotBeNull();
    }

    [Fact]
    public void Constructor_WithOptions_DoesNotThrow()
    {
        var options = new CopilotClientOptions { UseStdio = true };
        using var client = CreateClient(options: options);
        client.Should().NotBeNull();
    }

    [Fact]
    public void Constructor_WithLogger_DoesNotThrow()
    {
        var logger = new Mock<ILogger>().Object;
        using var client = CreateClient(logger: logger);
        client.Should().NotBeNull();
    }

    [Fact]
    public void Constructor_WithOptionsAndLogger_DoesNotThrow()
    {
        var options = new CopilotClientOptions();
        var logger = new Mock<ILogger>().Object;
        using var client = CreateClient(options: options, logger: logger);
        client.Should().NotBeNull();
    }

    #endregion

    #region Metadata

    [Fact]
    public void Metadata_ReturnsCorrectProviderName()
    {
        using var client = CreateClient();
        client.Metadata.ProviderName.Should().Be(nameof(CopilotChatClient));
    }

    [Fact]
    public void Metadata_ReturnsCorrectModelId()
    {
        const string model = "claude-sonnet-4.5";
        using var client = CreateClient(model: model);
        client.Metadata.DefaultModelId.Should().Be(model);
    }

    [Fact]
    public void Metadata_ProviderUri_IsNull()
    {
        using var client = CreateClient();
        client.Metadata.ProviderUri.Should().BeNull();
    }

    #endregion

    #region GetService

    [Fact]
    public void GetService_ReturnsNull_ForAnyServiceType()
    {
        using var client = CreateClient();

        client.GetService(typeof(IChatClient)).Should().BeNull();
        client.GetService(typeof(string)).Should().BeNull();
        client.GetService(typeof(object)).Should().BeNull();
    }

    [Fact]
    public void GetService_WithServiceKey_ReturnsNull()
    {
        using var client = CreateClient();
        client.GetService(typeof(IChatClient), "someKey").Should().BeNull();
    }

    #endregion

    #region Dispose / DisposeAsync

    [Fact]
    public void Dispose_CanBeCalledMultipleTimes()
    {
        var client = CreateClient();

        var act = () =>
        {
            client.Dispose();
            client.Dispose();
        };

        act.Should().NotThrow();
    }

    [Fact]
    public async Task DisposeAsync_CanBeCalledMultipleTimes()
    {
        var client = CreateClient();

        var act = async () =>
        {
            await client.DisposeAsync();
            await client.DisposeAsync();
        };

        await act.Should().NotThrowAsync();
    }

    #endregion

    #region GetResponseAsync - Disposed

    [Fact]
    public async Task GetResponseAsync_WhenDisposed_ThrowsObjectDisposedException()
    {
        var client = CreateClient();
        client.Dispose();

        var messages = new[] { new AIChatMessage(ChatRole.User, "Hello") };

        var act = () => client.GetResponseAsync(messages);

        await act.Should().ThrowAsync<ObjectDisposedException>();
    }

    [Fact]
    public async Task GetResponseAsync_WhenDisposedAsync_ThrowsObjectDisposedException()
    {
        var client = CreateClient();
        await client.DisposeAsync();

        var messages = new[] { new AIChatMessage(ChatRole.User, "Hello") };

        var act = () => client.GetResponseAsync(messages);

        await act.Should().ThrowAsync<ObjectDisposedException>();
    }

    #endregion

    #region GetStreamingResponseAsync - Disposed

    [Fact]
    public async Task GetStreamingResponseAsync_WhenDisposed_ThrowsObjectDisposedException()
    {
        var client = CreateClient();
        client.Dispose();

        var messages = new[] { new AIChatMessage(ChatRole.User, "Hello") };

        var act = async () =>
        {
            await foreach (var _ in client.GetStreamingResponseAsync(messages))
            {
                // Should not reach here
            }
        };

        await act.Should().ThrowAsync<ObjectDisposedException>();
    }

    [Fact]
    public async Task GetStreamingResponseAsync_WhenDisposedAsync_ThrowsObjectDisposedException()
    {
        var client = CreateClient();
        await client.DisposeAsync();

        var messages = new[] { new AIChatMessage(ChatRole.User, "Hello") };

        var act = async () =>
        {
            await foreach (var _ in client.GetStreamingResponseAsync(messages))
            {
                // Should not reach here
            }
        };

        await act.Should().ThrowAsync<ObjectDisposedException>();
    }

    #endregion

    #region IChatClient interface

    [Fact]
    public void ImplementsIChatClient()
    {
        using var client = CreateClient();
        client.Should().BeAssignableTo<IChatClient>();
    }

    [Fact]
    public void ImplementsIAsyncDisposable()
    {
        using var client = CreateClient();
        client.Should().BeAssignableTo<IAsyncDisposable>();
    }

    [Fact]
    public void ImplementsIDisposable()
    {
        using var client = CreateClient();
        client.Should().BeAssignableTo<IDisposable>();
    }

    #endregion
}
