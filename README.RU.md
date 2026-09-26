[English](README.md) | **Русский**

# Проект Hex Viewer

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)

Набор классов для просмотра данных в режиме HEX представления.
Базовая задача вьювера предоставить программисту реализацию наследников с целью компоновки практически любого желаемого стиля отображения.
Для этого разработана архитектура поддерживающая любые источники данных, как в виде самой модели данных, так и в рамках модели отображения.
Базовый класс THexView заточен на простое отображение переданного в него стрима с минимальным функционалом.
Расширенный TMappedHexView - охватывает полный спектр задач требующийся от данного типа контролов, работая через абстракцию RawData, которая выполняет роль модели.

### Установка:

Delphi - собрать и установить пакет FWHexView_D.dproj

Lazarus - собрать FWHexView.LCL.lpk, затем собрать и установить FWHexView_D.LCL.lpk

### Внешний вид:

Фреймворк ключает 4 демо приложения:

1. Показывает основную функциональность

![1](https://github.com/AlexanderBagel/FWHexView/blob/master/img/basic.png?raw=true "показывает основную функциональность")

2. Показывает работу с виртуальными страницами

![2](https://github.com/AlexanderBagel/FWHexView/blob/master/img/pages.png?raw=true "показывает работу с виртуальными страницами")

3. Пример HEX редактора основанного на FWHexView

![3](https://github.com/AlexanderBagel/FWHexView/blob/master/img/hexview_demo.png?raw=true "пример HEX редактора основанного на FWHexView")

4. Пример работы с картами памяти на базе просмотра ZIP-архива

![4](https://github.com/AlexanderBagel/FWHexView/blob/master/img/zipviewer.png?raw=true "пример работы с картами памяти на базе просмотра ZIP-архива")

### Лицензия:

Начиная с версии 2.0.16, FWHexView распространяется под лицензией MIT.
Полный текст лицензии находится в файле [LICENSE](LICENSE).

### Список изменений:

#### 2.0.17 (26-09-2026)
- Для более тонкого управления шириной колонок добавлено свойство OnAfterAutoSizeColumns
- Шрифты по умолчанию выбираются от их наличия. 'Consolas' меняется на 'Lucida Console' в Windows, 'DejaVu Sans Mono' меняется на 'Monospace' в Linux.
- Добавлен метод TFWCustomHexView.SelectAll
- При расчете ширины ctOpcode в TCustomMappedHexView теперь учитывается параметр ColumnMinWidth
- Исправление: функция ClearDataMap не очищала непосредственно DataMap
#### 2.0.16 (09-05-2026)
- Проект переведён на лицензию **MIT**.

### Поддержите проект

Если вы сочли этот проект полезным, вы можете поддержать его развитие на странице главного проекта автора:

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)