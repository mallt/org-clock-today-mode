[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## org-clock-today for emacs
Minor mode to show the total clocked time of the current day in the mode line.

<p align="center">
<img src="https://raw.github.com/mallt/org-clock-today-mode/master/org-clock-today-mode-line.png" alt="org-clock-today screenshot"/>
</p>

## Customization
The default org clock mode line will still be shown by default but can be hidden by setting the variable `org-clock-today-hide-default-org-clock-mode-line` to non-nil.

## Usage
- Enable the mode globally with `(org-clock-today-mode 1)`.
- When you clock in, the total of today will be visible in the mode line and will be refreshed every minute.
- When you clock out, the total will be hidden.



[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
