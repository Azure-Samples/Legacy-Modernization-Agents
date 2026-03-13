const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    const disposables = [];

    disposables.push(vscode.commands.registerCommand('legacyModernization.dashboard', () => {
        showDashboard(context);
    }));

    disposables.push(vscode.commands.registerCommand('legacyModernization.openPortal', () => {
        openPortalWebview(context);
    }));

    disposables.push(vscode.commands.registerCommand('legacyModernization.runDoctor', () => {
        runDoctor('run');
    }));

    disposables.push(vscode.commands.registerCommand('legacyModernization.runDoctorPortal', () => {
        runDoctor('portal');
    }));

    context.subscriptions.push(...disposables);
}

function deactivate() { }

function getWorkspaceRoot() {
    const folders = vscode.workspace.workspaceFolders;
    if (!folders || folders.length === 0) {
        vscode.window.showErrorMessage('No workspace is open. Please open the Legacy-Modernization-Agents workspace.');
        return undefined;
    }
    return folders[0].uri.fsPath;
}

function expandSetting(value, workspaceRoot) {
    if (!value) return value;
    return value.replace('${workspaceFolder}', workspaceRoot);
}

function readPrompts(promptsFolder) {
    const prompts = [];
    try {
        const files = fs.readdirSync(promptsFolder, { withFileTypes: true });
        for (const entry of files) {
            if (!entry.isFile()) continue;
            if (!entry.name.toLowerCase().endsWith('.md')) continue;
            const fullPath = path.join(promptsFolder, entry.name);
            const content = fs.readFileSync(fullPath, 'utf-8');
            const preview = content.split('\n').slice(0, 12).join('\n');
            prompts.push({
                name: entry.name.replace('.md', ''),
                fileName: entry.name,
                path: fullPath,
                preview
            });
        }
    } catch (err) {
        console.error('Failed to read prompts', err);
    }
    return prompts;
}

function showDashboard(context) {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) return;

    const cfg = vscode.workspace.getConfiguration('legacyModernization');
    const promptsFolder = expandSetting(cfg.get('promptsFolder'), workspaceRoot);
    const prompts = readPrompts(promptsFolder);

    const panel = vscode.window.createWebviewPanel(
        'legacyModernizationDashboard',
        'Legacy Modernization: Agent Prompts',
        vscode.ViewColumn.One,
        {
            enableScripts: true,
            retainContextWhenHidden: true
        }
    );

    panel.webview.html = getDashboardHtml(panel.webview, prompts, cfg);

    panel.webview.onDidReceiveMessage(async (message) => {
        switch (message.type) {
            case 'openPrompt': {
                const target = message.path;
                if (!target) return;
                const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(target));
                await vscode.window.showTextDocument(doc, { preview: false });
                break;
            }
            case 'openPortal': {
                await vscode.commands.executeCommand('legacyModernization.openPortal');
                break;
            }
            case 'runDoctor': {
                runDoctor(message.mode === 'portal' ? 'portal' : 'run');
                break;
            }
            case 'refresh': {
                const updated = readPrompts(promptsFolder);
                panel.webview.html = getDashboardHtml(panel.webview, updated, cfg);
                break;
            }
            default:
                break;
        }
    });
}

function runDoctor(mode) {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) return;
    const cfg = vscode.workspace.getConfiguration('legacyModernization');
    const scriptPath = expandSetting(cfg.get('doctorScriptPath'), workspaceRoot);
    if (!fs.existsSync(scriptPath)) {
        vscode.window.showErrorMessage(`doctor.sh not found at ${scriptPath}`);
        return;
    }
    const terminal = vscode.window.createTerminal({
        name: `doctor.sh ${mode}`,
        cwd: workspaceRoot
    });
    terminal.show(true);
    terminal.sendText(`chmod +x "${scriptPath}"`);
    terminal.sendText(`"${scriptPath}" ${mode}`);
}

function openPortalWebview(context) {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) return;
    const cfg = vscode.workspace.getConfiguration('legacyModernization');
    const baseUrl = cfg.get('portalBaseUrl') || `http://localhost:${cfg.get('portalPort') || 5028}`;

    const panel = vscode.window.createWebviewPanel(
        'legacyModernizationPortal',
        'Legacy Modernization: Portal',
        vscode.ViewColumn.Two,
        {
            enableScripts: true,
            retainContextWhenHidden: true
        }
    );

    panel.webview.html = getPortalHtml(panel.webview, baseUrl);
}

function getDashboardHtml(webview, prompts, cfg) {
    const nonce = getNonce();
    const promptCards = prompts.map(p => {
        const escapedPreview = escapeHtml(p.preview || '');
        return `<div class="card">
            <div class="card-header">
                <div class="card-title">${escapeHtml(p.name)}</div>
                <div class="card-subtitle">${escapeHtml(p.fileName)}</div>
            </div>
            <div class="card-body">
                <pre>${escapedPreview}</pre>
                <div class="card-actions">
              <button data-action="open" data-path="${escapeHtml(p.path)}">Open</button>
                </div>
            </div>
        </div>`;
    }).join('\n');

    const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<meta name="viewport" content="width=device-width, initial-scale=1.0" />
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource} https: http: data:; style-src 'unsafe-inline'; script-src 'nonce-${nonce}'; frame-src ${webview.cspSource} http: https:;">
<style>
:root {
  --bg: #0f172a;
  --panel: #111827;
  --card: #1f2937;
  --accent: #22c55e;
  --text: #e5e7eb;
  --muted: #9ca3af;
  --border: #27303f;
  font-family: "Inter", "Segoe UI", system-ui, -apple-system, sans-serif;
}
body {
  margin: 0; padding: 0; background: radial-gradient(circle at 20% 20%, #12213b, #0b1220 55%); color: var(--text);
}
.header {
  padding: 16px 20px; border-bottom: 1px solid var(--border); background: rgba(17,24,39,0.9); backdrop-filter: blur(6px);
  display: flex; align-items: center; justify-content: space-between; gap: 12px; flex-wrap: wrap;
}
.hstack { display: flex; gap: 8px; flex-wrap: wrap; align-items: center; }
button {
  background: linear-gradient(135deg, #22c55e, #16a34a);
  color: #0b1220; border: none; border-radius: 8px; padding: 10px 14px; font-weight: 600; cursor: pointer;
  box-shadow: 0 6px 20px rgba(34,197,94,0.25); transition: transform 0.1s ease, box-shadow 0.1s ease;
}
button.secondary { background: #1f2937; color: #e5e7eb; box-shadow: none; border: 1px solid var(--border); }
button:hover { transform: translateY(-1px); box-shadow: 0 10px 25px rgba(34,197,94,0.35); }
.container { padding: 16px; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 12px; }
.card {
  background: var(--card); border: 1px solid var(--border); border-radius: 12px; padding: 12px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.15);
}
.card-header { display: flex; flex-direction: column; gap: 4px; margin-bottom: 8px; }
.card-title { font-size: 16px; font-weight: 700; }
.card-subtitle { font-size: 12px; color: var(--muted); }
.card-body pre {
  background: #0b1220; color: var(--text); border-radius: 8px; padding: 10px; max-height: 180px; overflow: auto; white-space: pre-wrap;
}
.card-actions { margin-top: 8px; display: flex; gap: 8px; }
.badge { background: rgba(34,197,94,0.15); color: #bbf7d0; padding: 4px 8px; border-radius: 999px; font-size: 12px; }
</style>
</head>
<body>
  <div class="header">
    <div class="hstack">
      <span class="badge">Agent prompts</span>
      <strong>Review or edit prompts before starting doctor.sh</strong>
    </div>
    <div class="hstack">
      <button class="secondary" data-action="refresh">Refresh</button>
      <button class="secondary" data-action="openPortal">Open Portal</button>
      <button data-action="doctorRun">doctor.sh run</button>
      <button data-action="doctorPortal">doctor.sh portal</button>
    </div>
  </div>
  <div class="container">
    <div class="grid">
      ${promptCards || '<div class="card"><div class="card-title">No prompts found</div><div class="card-body">Check configuration: legacyModernization.promptsFolder</div></div>'}
    </div>
  </div>
<script nonce="${nonce}">
  const vscode = acquireVsCodeApi();
  document.querySelectorAll('button[data-action="open"]').forEach(btn => {
    btn.addEventListener('click', () => {
      const filePath = btn.getAttribute('data-path');
      vscode.postMessage({ type: 'openPrompt', path: filePath });
    });
  });
  document.querySelector('button[data-action="openPortal"]').addEventListener('click', () => {
    vscode.postMessage({ type: 'openPortal' });
  });
  document.querySelector('button[data-action="doctorRun"]').addEventListener('click', () => {
    vscode.postMessage({ type: 'runDoctor', mode: 'run' });
  });
  document.querySelector('button[data-action="doctorPortal"]').addEventListener('click', () => {
    vscode.postMessage({ type: 'runDoctor', mode: 'portal' });
  });
  document.querySelector('button[data-action="refresh"]').addEventListener('click', () => {
    vscode.postMessage({ type: 'refresh' });
  });
</script>
</body>
</html>`;
    return html;
}

function getPortalHtml(webview, url) {
    const nonce = getNonce();
    return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<meta name="viewport" content="width=device-width, initial-scale=1.0" />
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; frame-src http: https:; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
<style>
  body { margin: 0; padding: 0; background: #0b1220; color: #e5e7eb; font-family: 'Inter', 'Segoe UI', system-ui, sans-serif; }
  .bar { padding: 10px 12px; display: flex; justify-content: space-between; align-items: center; background: #111827; border-bottom: 1px solid #1f2937; }
  .bar button { background: linear-gradient(135deg,#22c55e,#16a34a); color: #0b1220; border: none; border-radius: 8px; padding: 8px 12px; font-weight: 600; cursor: pointer; }
  iframe { width: 100%; height: calc(100vh - 48px); border: none; background: #0b1220; }
</style>
</head>
<body>
  <div class="bar">
    <div><strong>Portal</strong> <span style="color:#9ca3af;">${escapeHtml(url)}</span></div>
    <div>
      <button onclick="document.getElementById('portal-frame').src='${escapeHtml(url)}?_cb='+Date.now()">Refresh</button>
      <button onclick="window.open('${escapeHtml(url)}','_blank')">Open in Browser</button>
    </div>
  </div>
  <iframe id="portal-frame" src="${escapeHtml(url)}?_cb=${Date.now()}"></iframe>
</body>
</html>`;
}

function escapeHtml(value) {
  if (value === undefined || value === null) return '';
    return value
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

function getNonce() {
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let text = '';
    for (let i = 0; i < 16; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
}

module.exports = {
    activate,
    deactivate
};
