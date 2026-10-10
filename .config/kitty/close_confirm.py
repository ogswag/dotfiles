import os

from kitty.boss import Boss
from kitty.child import cached_process_data
from kittens.tui.handler import result_handler

IGNORE = frozenset({
    'ash', 'bash', 'csh', 'dash', 'elvish', 'fish', 'ion', 'kitten', 'kitty',
    'ksh', 'login', 'mksh', 'nu', 'oksh', 'powershell', 'pwsh', 'rc', 'sh',
    'tcsh', 'xonsh', 'yash', 'zsh',
})


def _name(cmdline) -> str:
    if not cmdline:
        return ''
    return os.path.basename(cmdline[0]).lstrip('-').lower()


def _running(window) -> list[str]:
    names: list[str] = []
    if window.has_running_program:
        fg = _name(window.child.foreground_cmdline)
        if fg not in IGNORE:
            names.append(fg or 'program')
    for proc in window.child.background_processes:
        name = _name(proc.get('cmdline'))
        if name and name not in IGNORE:
            names.append(name)
    return names


def main(args: list[str]) -> None:
    pass


@result_handler(no_ui=True)
def handle_result(args: list[str], answer: str, target_window_id: int, boss: Boss) -> None:
    mode = args[1] if len(args) > 1 else 'window'
    window = boss.window_id_map.get(target_window_id)
    if window is None:
        return
    tab = window.tabref()
    if mode == 'tab':
        if tab is None:
            return
        windows = list(tab)
        title = 'Close tab?'
        prompt = 'Are you sure you want to close this tab?'

        def close(ok: bool) -> None:
            if ok:
                boss.close_tab_no_confirm(tab)
    else:
        windows = [window]
        title = 'Close window?'
        prompt = 'Are you sure you want to close this window?'

        def close(ok: bool) -> None:
            if ok:
                boss.mark_window_for_close(window)

    with cached_process_data():
        names = [n for w in windows for n in _running(w)]
    if not names:
        close(True)
        return
    extra = len(names) - 1
    if extra:
        msg = f'{prompt} It is running: {names[0]} and {extra} other programs.'
    else:
        msg = f'{prompt} It is running: {names[0]}.'
    boss.confirm(msg, close, window=window, title=title)
