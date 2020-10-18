# 5x7 Dactyl Manuform

This is a fork of the [Dactyl-ManuForm](https://github.com/tshort/dactyl-keyboard), with code borrowed from the [Dactyl Manuform Mini](https://github.com/l4u/dactyl-manuform-mini-keyboard). The Dactyl-Manuform is a fork of the [Dactyl](https://github.com/adereth/dactyl-keyboard) with the thumb cluster from [ManuForm](https://github.com/jeffgran/ManuForm).

## Forks

- https://github.com/lebastaq/dactyl-manuform-mini-keyboard
- https://github.com/okke-formsma/dactyl-manuform-tight

## Features

- The use of sidenubs can be disabled. Sidenub should be disabled if you use Kailh, and Outemu. If you use Cherry MX, Gateron or Zealios switches, you can enable the sidenubs.
- Spaces for rentention tabs are added.
- One key in the thumb cluster has been removed. Other thumb keys are also adjusted.
- The total height is reduced and wire posts are removed. This results in a
  higher printing speed and a lower cost.
- A TRRS mount instead of a RJ9 mount is used.
- A USB-C breakout board mount is used.
- Screw posts are moved inside. The holes are designed for TRISERT® thread inserts 145m3.
- The pro micro holder has been modified. Dupont cables can be used.
- The base plate has been modified to accommodate a custom made palm rest
- Hole for reset switch

## Generate OpenSCAD and STL models

- Run `lein generate` or `lein auto generate`
- This will regenerate the `things/*.scad` files
- Use OpenSCAD to open a `.scad` file.
- Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
- When done, use OpenSCAD to export STL files

## BOM

- Key switches
- Keycaps
- TRRS Cable
- 2x [Arduino Pro Micro](https://www.digikey.com/en/products/detail/DEV-12640/1568-1060-ND/5140825)
- 2x [USB-C Breakout Board](https://www.digikey.com/en/products/detail/BOB-15100/1568-1958-ND/9770720)
- [Heat set inserts](https://www.digikey.com/en/products/detail/4255/1528-4255-ND/10244656)
- [E-Z LOK Threaded inserts](https://www.amazon.com/gp/product/B015CAPTZI) - for mounting the wooden wrist rest
- [TRRS Jacks](https://www.amazon.com/gp/product/B07KY7CJCJ)
- [M3x8mm machine screws](https://www.amazon.com/gp/product/B018RSV2AI)
- [Header pins](https://www.amazon.com/gp/product/B074HVBTZ4)
- [4.7 Ohm Resistors](https://www.amazon.com/gp/product/B0185FIIVE)
- [24 AWG hookup wire](https://www.digikey.com/en/products/detail/24UL1007SLDKIT/2328-24UL1007SLDKIT-ND/11614213)
- [IN4148 Diodes](https://www.digikey.com/en/products/detail/1N4148-TAP/1N4148-TAPCT-ND/3104296)
- [Silicone Jumper Wires](https://www.digikey.com/en/products/detail/4447/1528-4447-ND/11503291)
- 2x [Reset switches](https://www.digikey.com/en/products/detail/PS1024ARED/EG2015-ND/44577)
- 2x [USB Cable](https://www.digikey.com/en/products/detail/SC-2AMK003F/380-1431-ND/8544577)

Also not technically part of the BOM, but these Irwin wire strippers are the bomb: https://www.amazon.com/gp/product/B000OQ21CA

## Assembly notes

- None of the screw holes are countersunk but I used countersink screws since they are easier to come by than wafer headed screws. In order to make the countersink you can apply the same methodology you would to install heat set insets. That is, use a soldering iron to heat the screw while it is in the hole, applying pressure as you do. The screw will melt the surrounding plastic and form a nice countersink. For best results do this in two passes, clearing the plastic melt with a sharp knife between passes.
- [This build log](https://www.beekeeb.com/dactyl-manuform-mini-mechanical-keyboard-build-log/) linked in the original Dactyl Manuform Mini repo is super helpful

## License

Copyright © 2015-2020 Matthew Adereth, Tom Short, Leo Lou and Bruce Jones

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
