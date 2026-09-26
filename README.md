**English** | [Русский](README.RU.md)

# Hex Viewer Project

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)

A set of classes for viewing data in HEX representation mode.
The basic task of the viewer is to provide the programmer with an implementation of inheritors in order to compose almost any desired display style.
For this purpose, the architecture is designed to support any data source, both as a data model itself and as part of the display model.
The base class THexView is designed to simply display the stream passed to it with minimal functionality.
Extended TMappedHexView - covers the full range of tasks required from this type of controls, working through the RawData abstraction, which performs the role of a model.

### Setup:

Delphi - build and install FWHexView_D.dproj

Lazarus - build FWHexView.LCL.lpk, than build and install FWHexView_D.LCL.lpk

### Appearance:

The framework includes 4 demo applications:

1. Shows the overall functionality

![1](https://github.com/AlexanderBagel/FWHexView/blob/master/img/basic.png?raw=true "shows the overall functionality")

2. Shows work with virtual pages

![2](https://github.com/AlexanderBagel/FWHexView/blob/master/img/pages.png?raw=true "shows work with virtual pages")

3. Example of hex editor based on FWHexView

![3](https://github.com/AlexanderBagel/FWHexView/blob/master/img/hexview_demo.png?raw=true "example of hex editor based on FWHexView")

4. Example of working with memory cards using the example of viewing a ZIP archive

![4](https://github.com/AlexanderBagel/FWHexView/blob/master/img/zipviewer.png?raw=true "example of working with memory cards using the example of viewing a ZIP archive")

### License:

Starting from version 2.0.16, FWHexView is distributed under the MIT License.
See the [LICENSE](LICENSE) file for full license text.

### Changelog:

#### 2.0.17 (26-05-2026)
- The OnAfterAutoSizeColumns property has been added to allow for more precise control over column widths.
- Default fonts are selected based on availability. “Consolas” is replaced with “Lucida Console” on Windows, and “DejaVu Sans Mono” is replaced with “Monospace” on Linux.
- Added the TFWCustomHexView.SelectAll method
- ColumnMinWidth is now taken into account when calculating the width of ctOpcode in TCustomMappedHexView
- BugFix: ClearDataMap did not clear the DataMap itself

#### 2.0.16 (09-05-2026)
- The project has been relicensed to the **MIT License**.

### Support the Project

If you found this project useful, you can support its development on the author's main-project page:

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)