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
- 2x [nice!nano](https://nicekeyboards.com/nice-nano/)
- [Heat set inserts](https://www.digikey.com/en/products/detail/4255/1528-4255-ND/10244656)
- [TRRS Jacks](https://www.amazon.com/gp/product/B07KY7CJCJ)
- [M3x8mm machine screws](https://www.amazon.com/gp/product/B018RSV2AI)
- [Header pins](https://www.amazon.com/gp/product/B074HVBTZ4)
- [24 AWG hookup wire](https://www.digikey.com/en/products/detail/24UL1007SLDKIT/2328-24UL1007SLDKIT-ND/11614213)
- [IN4148 Diodes](https://www.digikey.com/en/products/detail/1N4148-TAP/1N4148-TAPCT-ND/3104296)
- [Jumper Wires](https://www.adafruit.com/product/1949)
- 2x [Reset switches](https://www.amazon.com/gp/product/B07F9PLSRY/ref=ppx_yo_dt_b_asin_title_o00_s00?ie=UTF8&psc=1)
- 2x [Power switches](https://www.amazon.com/gp/product/B07F9K3F82/ref=ppx_yo_dt_b_asin_title_o04_s00?ie=UTF8&psc=1)
- [JST-PH 2.0 Female Connectors](https://www.amazon.com/gp/product/B07NWNPB77/ref=ppx_yo_dt_b_asin_title_o00_s00?ie=UTF8&psc=1)
- [JST-PH 2.0 Male Connectors](https://www.amazon.com/BDHI-Electrical-Connector-Silicone-B184-20/dp/B096D8SCP6/ref=sr_1_11?)
- OPTIONAL: [E-Z LOK Threaded inserts](https://www.amazon.com/gp/product/B015CAPTZI) - for mounting the wooden palm rest
- OPTIONAL: Wood stock - for making the wooden

Also not technically part of the BOM, but these Irwin wire strippers are the bomb: https://www.amazon.com/gp/product/B000OQ21CA

## Build notes

- Use my [ZMK config](https://github.com/brucedjones/zmk-config). You may need to adjust the pins used for rows/columns on the nicenano. In my config I use D5-9 for rows and A0-2, D10, D16, D14, D15 for the columns.
- [This build log](https://www.beekeeb.com/dactyl-manuform-mini-mechanical-keyboard-build-log/) linked in the original Dactyl Manuform Mini repo is super helpful
- The palm rest is optional, some notes on how to make one are found in [this imgur](https://imgur.com/gallery/1zfyPCG) album from my first dactyl build
- To extend the base plate to accomodate a palm rest, set `add-palm-rest` to `true` in the clojure code.

## License

Copyright © 2015-2021 Matthew Adereth, Tom Short, Leo Lou and Bruce Jones

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
