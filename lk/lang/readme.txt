Quick i18n/l10n instructions:

1. Install xgettext for windows from: http://gnuwin32.sourceforge.net/packages/gettext.htm
2. Add c:\Program Files (x86)\Gnuwin32\bin to the system PATH
3. In the lk/src folder, run:    xgettext -d lk -s --keyword=lk_tr -o lk.pot *.cpp
4. Copy the .pot file to the lang folder, and update the .po in the various translation folders
5. Translate using poEdit (https://poedit.net/)
