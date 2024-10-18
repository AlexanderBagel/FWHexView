# Hex Viewer Project

Attention - BETA, version!!!

Not for sale - the debugging process is underway.

A set of classes for viewing data in HEX representation mode.
The basic task of the viewer is to provide the programmer with an implementation of inheritors in order to compose almost any desired display style.
For this purpose, the architecture is designed to support any data source, both as a data model itself and as part of the display model.
The base class THexView is designed to simply display the stream passed to it with minimal functionality.
Extended TMappedHexView - covers the full range of tasks required from this type of controls, working through the RawData abstraction, which performs the role of a model.

### Внимание - BETA, версия!!!
Не для продажи - идет процесс отладки.

Набор классов для просмотра данных в режиме HEX представления.
Базовая задача вьювера предоставить программисту реализацию наследников с целью компоновки практически любого желаемого стиля отображения.
Для этого разработана архитектура поддерживающая любые источники данных, как ввиде самой модели данных, так и в рамках модели отображения.
Базовый класс THexView заточен на простое отобрадение переданного в него стрима с минимальным функционалом.
Расширенный TMappedHexView - охватывает полный спектр задач требующийся от данного типа контролов, работая через абстракцию RawData, которая выполняет роль модели.

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

### Licence:
  FWHexView is dual-licensed. You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence. A commercial licence grants you the right to use FWHexView in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FWHexView
  to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge. A commercial licence is sold per developer developing applications that use FWHexView, as follows:
<table>
<tr><td>Number Of Developers</td><td>Price (USD)</td></tr>
<tr><td>1 developer</td><td>$49</td></tr>
<tr><td>2 developers</td><td>$89</td></tr>
<tr><td>3 developers</td><td>$139</td></tr>
<tr><td>4 developers</td><td>$169</td></tr>
<tr><td>5 developers</td><td>$199</td></tr>
<tr><td>More than 5 developers</td><td>$199 + $25 per developer from the 6th onwards</td></tr>
<tr><td>Site licence (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)</td><td>$499</td></tr>
</table>  

  Please send an e-mail to rouse79@yandex.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.  
  
### Лицензия:
  FWHexView имеет двойную лицензию. Вы можете использовать его в соответствии с ограничениями лицензии GPL v3 без каких-либо затрат,
  или приобрести коммерческую лицензию. Коммерческая лицензия предоставляет вам право использовать FWHexView в ваших собственных
  приложениях, безвозмездно и без требования раскрывать свой исходный код или любые модификации FWHexView
  любой другой стороне. Коммерческая лицензия действует бессрочно и дает вам право на все последующие бесплатные обновления.
  бесплатно. Коммерческая лицензия продается на одного разработчика, разрабатывающего приложения, использующие FWHexView, следующим образом:
<table>
<tr><td>Number Of Developers</td><td>Price (USD)</td></tr>
<tr><td>1 разработчик</td><td>$49</td></tr>
<tr><td>2 разработчика</td><td>$89</td></tr>
<tr><td>3 разработчика</td><td>$139</td></tr>
<tr><td>4 разработчика</td><td>$169</td></tr>
<tr><td>5 разработчиков</td><td>$199</td></tr>
<tr><td>>5 разработчиков</td><td>$199 + $25 за каждого разработчика, начиная с 6-го</td></tr>
<tr><td>Site licence (неограниченное количество разработчиков, связанных с владельцем лицензии, т.е. сотрудников, коллег, стажеров и подрядчиков)</td><td>$499</td></tr>
</table>    

  Пожалуйста, отправьте письмо на rouse79@yandex.ru, чтобы запросить счет до или после оплаты. Оплата может быть осуществлена 
  через банковский перевод. Банковские реквизиты будут указаны в счете-фактуре.

  Поддержка (по электронной почте) доступна для пользователей с коммерческой лицензией. Запросы на усовершенствование, поданные пользователями с
  коммерческой лицензией, будут приоритетными.     