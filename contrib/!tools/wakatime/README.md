# Wakatime contribution layer for Spacemacs

![logo_wakatime](img/wakatime.png) 

## Description

This layer adds support for Wakatime.

WakaTime was built to solve time tracking for programmers.

Since we work inside a text editor, why should we have to start and stop a timer? WakaTime uses open-source text editor plugins to automatically track the time you spend programming so you never have to manually track it again!

P.S. wakati means time in Swahili

## Install

### Wakatime Program

You can follow wakatime installation instructions here
https://github.com/wakatime/wakatime-mode.
But in short it's just:
```sh
pip install wakatime
```
For some linux users
```sh
sudo pip install wakatime
```

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(wakatime))
```
### API Keys

After this go to your wakatime account and have your API key handy https://wakatime.com/settings/account?apikey=true .

Restart emacs and it will prompt you for the location of the wakatime installer
(just put in whatever `which wakatime` gives you in the terminal e.g.
`/usr/bin/wakatime` for example) and that's it.



## Note to `venv-workon` users:

Right now wakatime uses `python` as python bin executable, so if you use
`venv-workon` because you have python projects which need to have a virtual env,
then wakatime, which is installed system-wide will have trouble locating
wakatime files, so it's best to define by yourself the python path where
wakatime can always find it's stuff, via this variable:


```elisp
(setq wakatime-python-bin "/path/to/python")
```
