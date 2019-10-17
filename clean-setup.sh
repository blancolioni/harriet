#!/bin/sh
./build/bin/harriet-driver --create
#./build/bin/harriet-driver --add-faction --account-name=root --faction-name=Republic --faction-adjective=Republican --faction-plural=Republicans --faction-color=#003153 --faction-setup-path=capital
./build/bin/harriet-driver --add-faction --faction-names-file=faction-names.config --faction-colors-file=faction-colors.config
echo Saving initial state
cp harriet.marlowe harriet-init.marlowe
