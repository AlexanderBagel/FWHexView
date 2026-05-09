////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Cairo.pas
//  * Purpose   : Speeding up text output with the Cairo library
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2026.
//  * Version   : 2.0.16
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//  *
//  * SPDX-License-Identifier: MIT
//  * See LICENSE file in the project root for full license information.
//  *
//  ****************************************************************************
//

unit FWHexView.Cairo;

{$mode Delphi}

{-$DEFINE DEBUG_CAIRO_DUMP}

interface

uses
  LCLType,
  LCLIntf,
  Types,
  SysUtils,
  Graphics,
  Classes,
  Generics.Collections,
  Generics.Defaults,
  Generics.Hashes,
  StrUtils,
  Math,
  UnixType,
  gdk2,
  Gtk2Def,
  Cairo,
  CairoFT,
  freetypeh,
  libfontconfig;

type
  TUnicodeScript = (
    usUnknown, usLatin, usCyrillic, usGreek, usArmenian, usGeorgian, usHebrew,
    usArabic, usSyriac, usThaana, usDevanagari, usBengali, usGurmukhi,
    usGujarati, usOriya, usTamil, usTelugu, usKannada, usMalayalam, usSinhala,
    usThai, usLao, usTibetan, usMyanmar, usKhmer, usEthiopic, usCherokee,
    usCanadianAboriginal, usOgham, usRunic, usMongolian, usHiragana, usKatakana,
    usHangul, usHan, usYi, usBopomofo, usTagalog, usHanunoo, usBuhid,
    usTagbanwa, usBuginese, usBalinese, usSundanese, usJavanese, usLimbu,
    usTaiLe, usNewTaiLue, usTaiTham, usLepcha, usOlChiki, usVai, usBamum,
    usSylotiNagri, usCham, usTaiViet, usMeeteiMayek, usNko, usTifinagh,
    usCJKCompatibility, usEmoji, usMathematical, usSymbols,
    usPrivateUse, usBraille, usBrahmi, usAvestan, usEgyptianHieroglyphs,
    usCuneiform, usLinearB, usPhoenician, usInherited);
  TUnicodeScripts = set of TUnicodeScript;

  TCairoScaledFontType = (csftRegular, csftBold, csftItalic, csftBoldItalic);

  // структура описывающая установленный в системе шрифт
  
  // structure describing a font installed in the system  
  TFontFaceData = record
    FontPath: string;
    FontFamily: string;
    FontStyle: string;
    FaceIndex: Integer;
    Slant, Width, Weight: Integer;
    Scripts: TUnicodeScripts;
	
    // хэндлы FontFace и Cairo шрифтов привязаны к дескриптору.
    // загруженные шрифты из карты, информация о которых хранится в TFontMapFontDescriptor
    // сами ничего не хранят и ссылаются на текущую структуру
	
    // FontFace and Cairo font handles are bound to the descriptor.
    // Loaded fonts from the map, whose information is stored in TFontMapFontDescriptor
    // do not store anything themselves and reference the current structure	
	
    FaceHandles: array [Boolean] of PFT_Face;
    FontHandles: array [TCairoScaledFontType] of Pcairo_scaled_font_t;
  end;

  // структрура для быстрого поиска загруженого шрифта по переданному TFont
  
  // structure for quick search of loaded font by given TFont  
  TFontMapFontInfo = record
    Name: string;
    Bold, Italic, StrikeOut, Underline: Boolean;
    Script: TUnicodeScript;
  end;

  { TFontMapFontInfoComparer }

  TFontMapFontInfoComparer = class(TInterfacedObject, IEqualityComparer<TFontMapFontInfo>)
  public
    function Equals(const Left, Right: TFontMapFontInfo): Boolean; reintroduce;
    function GetHashCode(const Value: TFontMapFontInfo): LongWord; reintroduce;
  end;

  // структура хранящая корректирующие параметры FontFace
  // которые необходимо применить к создаваемому CairoFont
  
  // structure storing corrective FontFace parameters
  // that need to be applied to the created CairoFont  
  TFontFaceDescriptor = record
    Index: Integer;
    PixelSize: Double;
    Embolden: TFcBool;
    FontType: TCairoScaledFontType;
  end;

  // структура хранящая служебные параметры загруженного шрифта

  // structure storing service parameters of loaded font  
  TFontMapFontDescriptor = record
    ff_desc: TFontFaceDescriptor;
	
    // использовать ТОЛЬКО для назначения шрифта, не для реальной работы!
	
    // use ONLY for font assignment, not for actual work!
	
    scaled_font: Pcairo_scaled_font_t;
	
    // устанавливаются после выставления масштаба в cairo_set_font_descriptor
	
    // set after setting scale in cairo_set_font_descriptor
	
    extents: cairo_font_extents_t;
    baseline: Integer;
	
    // кэш символов отсутствующих в текущем шрифте
	
    // cache of characters missing in the current font
	
    cache: TDictionary<TFcChar32, Integer>;
  end;

  // Классы для вывода статистики по кэшу шрифтов

  // Classes for outputting font cache statistics
  TFontFamilyStat = record
    Name: string;
    FontsCount, LoadedFontFaceCount, LoadedFontsCount: Integer;
  end;

  TCairoFontMapStat = record
    KnownFontCount: Integer;
    LoadedFontDescriptorCount: Integer;
    AssotiatedFontInfoCount: Integer;
    TotalFontHandles: Integer;
    FontFamilyStat: array of TFontFamilyStat;
  end;

  PCairoClusterArray = ^TCairoClusterArray;
  TCairoClusterArray = array [0..0] of cairo_text_cluster_t;

  // структура хранящая служебные параметры кайро контекста

  // structure storing service parameters of cairo context  
  TCairoContext = record
    Context: pcairo_t;
    Font: TFontMapFontDescriptor;
    Origin: TPoint;
    Glyphs: Pcairo_glyph_t;
    GlyphsLen: LongInt;
    Clusters: PCairoClusterArray;
    ClustersLen: LongInt;
    InvalidGlyphsPresent: Boolean;
    VisibleGlyphsPresent: Boolean;
  end;

  TListOfInteger = class(TList<Integer>);

  ECairoException = class(Exception);

  { TCairoFontMap }

  TCairoFontMap = class
  private class var
    FInstance: TCairoFontMap;
    class constructor CairoFontMapCreate;
    class destructor CairoFontMapDestroy;
  protected
    FFontFaceList: array of TFontFaceData;
  strict private
    FFastGlyphSearch: Boolean;
    FFontFaceFamily: TObjectDictionary<string, TListOfInteger>;
    FFontPathList: TStringList;
    FLoadedFontList: TList<TFontMapFontDescriptor>;
    FLoadedFonts: TDictionary<TFontMapFontInfo, Integer>;
    FLoadedFontHandles: TDictionary<Pcairo_scaled_font_t, Integer>;
    FLibrary: PFT_Library;
    function CreateFontDescriptor(AFont: TFont;
      const AFontFace: TFontFaceDescriptor): TFontMapFontDescriptor;
    procedure ExtractFontParam(AFont: TFont; out AFace, AStyle: string;
      out ASlant, AWeight, AWidth: Integer);
    function GetFontFaceIndex(const FontPath: string): Integer;
    function FindMatchFont(AFont: TFont; UnicodeChar: TFcChar32;
      out AFontFaceDescriptor: TFontFaceDescriptor): Boolean;
    function GetFT_Face(const AFontFace: TFontFaceDescriptor): PFT_Face;
    function GetFontInfo(AFont: TFont): TFontMapFontInfo;
    procedure InitFonts;
    procedure ReleaseFontFaceList;
    procedure SetFastGlyphSearch(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCache;
    function CollectStat: TCairoFontMapStat;
    function GetCairoFont(AFont: TFont): TFontMapFontDescriptor;
    function GetCairoCharFont(AFont: TFont; Str: PChar): TFontMapFontDescriptor;
    property FastGlyphSearch: Boolean read FFastGlyphSearch write SetFastGlyphSearch;
  end;

  function CairoFontMap: TCairoFontMap;

  function CairoDrawText(ACanvas: TCanvas; const Str: string;
    var ARect: TRect; Flags: Cardinal): Integer;
  function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

implementation

{$REGION ' Lang Consts '}

const
  ScriptToLangData: array [TUnicodeScript] of string = (
    '', 'en', 'ru', 'el', 'hy', 'ka', 'he', 'ar', 'syr', 'dv', 'hi', 'bn', 'pa',
    'gu', 'or', 'ta', 'te', 'kn', 'ml', 'si', 'th', 'lo', 'bo', 'my', 'km',
    'am', 'chr', 'iu', 'sga', 'non', 'mn', 'ja', 'ja', 'ko', 'zh', 'ii', 'zh',
    'tl', 'hnn', 'bku', 'tbw', 'bug', 'ban', 'su', 'jv', 'lif', 'tdd', 'khb',
    'nod', 'lep', 'sat', 'vai', 'bax', 'syl', 'cjm', 'tyj', 'mni', 'nqo',
    'ber', 'zh', 'emoji', 'math', 'sym', 'pua', 'braille', 'brah', 'ae', 'egy',
    'sux', 'gmy', 'phn', 'inherited'
  );

  LatinLangs: array[0..137] of string = (
    'en', 'af', 'sq', 'ast', 'eu', 'br', 'ca', 'co', 'cs', 'da', 'nl', 'eo',
    'et', 'fo', 'fi', 'fr', 'fur', 'fy', 'gl', 'de', 'gv', 'hu', 'is', 'id',
    'ga', 'it', 'kw', 'la', 'lb', 'lt', 'lv', 'mg', 'ms', 'mt', 'nb', 'nn',
    'no', 'oc', 'pl', 'pt', 'ro', 'rm', 'sco', 'gd', 'sh', 'sk', 'sl', 'es',
    'sw', 'sv', 'tl', 'fil', 'tr', 'vo', 'cy', 'wa', 'wen', 'aa', 'bin', 'ee',
    'fat', 'ha', 'ig', 'lg', 'rw', 'sn', 'st', 'tn', 'ts', 've',
    'xh', 'yo', 'zu', 'ak', 'bm', 'ff', 'ki', 'kj', 'kr', 'ln', 'ng', 'nr',
    'nso', 'ny', 'rn', 'sg', 'ss', 'tw', 'wo', 'hz', 'ia', 'ie', 'io', 'ay',
    'bi', 'ch', 'fj', 'gn', 'haw', 'ho', 'ht', 'ik', 'kwm', 'mh', 'mi', 'na',
    'nv', 'qu', 'quz', 'sm', 'to', 'ty', 'an', 'csb', 'hsb', 'nds', 'sc', 'shs',
    'li', 'pap-an', 'pap-aw', 'sma', 'smj', 'smn', 'sms', 'se',
    'vot', 'yap', 'za', 'crh', 'ku-tr', 'az-az', 'ku-am',
    'tk', 'vi', 'bs', 'hr', 'kl'
  );

  CyrillicLangs: array[0..28] of string = (
    'ru', 'be', 'bg', 'mk', 'sr', 'uk', 'cu', 'kk', 'ky', 'mn-mn', 'tg', 'tt',
    'uz', 'ba', 'bua', 'ce', 'chm', 'cv', 'kv', 'kum', 'lez', 'sah', 'tyv', 'mo',
    'ab', 'os', 'av', 'kaa', 'sel'
  );

  HebrewLangs: array[0..1] of string = ('he', 'yi');

  ArabicLangs: array[0..12] of string = (
    'ar', 'fa', 'ur', 'ps-af', 'ps-pk', 'ku-iq', 'ku-ir', 'ks', 'sd', 'az-ir',
    'ug', 'lah', 'ota'
  );

  DevanagariLangs: array[0..10] of string = (
    'hi', 'ne', 'mr', 'kok', 'sa', 'bho', 'bh', 'brx', 'doi', 'hne', 'mai'
  );

  GurmukhiLangs: array[0..1] of string = ('pa', 'pa-pk');


  EthiopicLangs: array[0..9] of string = (
    'am', 'ti-er', 'ti-et', 'gez', 'byn', 'sid', 'wal', 'tig', 'om', 'so'
  );

  HanLangs: array[0..3] of string = ('zh', 'zh-cn', 'zh-sg', 'zh-tw');

  TifinaghLangs: array[0..3] of string = ('ber', 'ber-dz', 'ber-ma', 'kab');

  MathematicalLangs: array[0..1] of string = ('math', 'und-zmth');

  SingleLangs: array[0..21] of string = (
    'el', 'hy', 'ka', 'syr', 'dv', 'bn', 'gu', 'or', 'ta', 'te', 'kn', 'ml',
    'si', 'th', 'lo', 'bo', 'my', 'km', 'chr', 'zh-bopo', 'braille', 'emoji'
  );

  FontWeights: array [0..18] of string = (
    'thin', 'extralight', 'ultralight', 'light', 'demilight', 'semilight',
    'book', 'regular', 'normal', 'medium', 'demibold', 'semibold', 'bold',
    'extrabold', 'ultrabold', 'black', 'heavy', 'extrablack', 'ultrablack'
  );

  FontWidths: array [0..8] of string = (
    'ultracondensed', 'extracondensed', 'condensed', 'semicondensed', 'normal',
    'semiexpanded', 'expanded', 'extraexpanded', 'ultraexpanded'
  );

{$ENDREGION}

{$REGION ' Script Utils '}

function IsInvisibleChar(AChar: TFcChar32): Boolean;
begin
  case AChar of
    9, $A..$D, $20, $A0, $85, $1680, $2000..$200F,
    $2028..$202F, $205F..$2064, $3000, $FEFF:
      Result := True;
  else
    Result := False;
  end;
end;

function CharToScript(UnicodeChar: TFcChar32): TUnicodeScript;
begin
  case UnicodeChar of
    $0000..$007F, $0080..$00FF, $0100..$017F, $0180..$024F, $0250..$02AF,
    $02B0..$02FF, $1D00..$1D7F, $1D80..$1DBF, $1E00..$1EFF, $2C60..$2C7F,
    $2C80..$2CFF, $A720..$A7FF, $AB30..$AB6F:
      Result := usLatin;
    $0370..$03FF, $1F00..$1FFF, $10140..$1018F:
      Result := usGreek;
    $0400..$04FF, $0500..$052F, $2DE0..$2DFF, $A640..$A69F, $1C80..$1C8F:
      Result := usCyrillic;
    $0530..$058F: Result := usArmenian;
    $0590..$05FF: Result := usHebrew;
    $0600..$06FF, $0750..$077F, $08A0..$08FF, $FB50..$FDFF, $FE70..$FEFF:
      Result := usArabic;
    $0700..$074F: Result := usSyriac;
    $0780..$07BF: Result := usThaana;
    $0900..$097F: Result := usDevanagari;
    $0980..$09FF: Result := usBengali;
    $0A00..$0A7F: Result := usGurmukhi;
    $0A80..$0AFF: Result := usGujarati;
    $0B00..$0B7F: Result := usOriya;
    $0B80..$0BFF: Result := usTamil;
    $0C00..$0C7F: Result := usTelugu;
    $0C80..$0CFF: Result := usKannada;
    $0D00..$0D7F: Result := usMalayalam;
    $0D80..$0DFF: Result := usSinhala;
    $0E00..$0E7F: Result := usThai;
    $0E80..$0EFF: Result := usLao;
    $0F00..$0FFF: Result := usTibetan;
    $1000..$109F: Result := usMyanmar;
    $1780..$17FF, $19E0..$19FF: Result := usKhmer;
    $10A0..$10FF, $2D00..$2D2F: Result := usGeorgian;
    $1200..$137F, $1380..$139F, $2D80..$2DDF: Result := usEthiopic;
    $13A0..$13FF: Result := usCherokee;
    $1400..$167F: Result := usCanadianAboriginal;
    $1680..$169F: Result := usOgham;
    $16A0..$16FF: Result := usRunic;
    $1800..$18AF: Result := usMongolian;
    $1700..$171F: Result := usTagalog;
    $1720..$173F: Result := usHanunoo;
    $1740..$175F: Result := usBuhid;
    $1760..$177F: Result := usTagbanwa;
    $1900..$194F: Result := usLimbu;
    $1950..$197F: Result := usTaiLe;
    $1980..$19DF: Result := usNewTaiLue;
    $1A00..$1A1F: Result := usBuginese;
    $1A20..$1AAF: Result := usTaiTham;
    $1B00..$1B7F: Result := usBalinese;
    $1B80..$1BBF: Result := usSundanese;
    $1C00..$1C4F: Result := usLepcha;
    $1C50..$1C7F: Result := usOlChiki;
    $07C0..$07FF: Result := usNko;
    $2D30..$2D7F: Result := usTifinagh;
    $A800..$A82F: Result := usSylotiNagri;
    $A500..$A63F: Result := usVai;
    $A6A0..$A6FF: Result := usBamum;
    $A980..$A9DF: Result := usJavanese;
    $AA00..$AA5F: Result := usCham;
    $AA80..$AADF: Result := usTaiViet;
    $ABC0..$ABFF: Result := usMeeteiMayek;
    $3040..$309F: Result := usHiragana;
    $30A0..$30FF, $31F0..$31FF: Result := usKatakana;
    $1100..$11FF, $3130..$318F, $A960..$A97F, $AC00..$D7AF, $D7B0..$D7FF:
      Result := usHangul;
    $4E00..$9FFF, $3400..$4DBF, $20000..$2A6DF, $2A700..$2B73F,
    $2B740..$2B81F, $2B820..$2CEAF:
      Result := usHan;
    $A000..$A48F, $A490..$A4CF: Result := usYi;
    $3100..$312F, $31A0..$31BF: Result := usBopomofo;
    $2800..$28FF: Result := usBraille;
    $1F600..$1F64F, $1F300..$1F5FF, $1F680..$1F6FF, $1F900..$1F9FF,
    $1FA00..$1FA6F:
      Result := usEmoji;
    $2070..$209F, $2100..$214F, $2150..$218F, $2190..$21FF, $2200..$22FF,
    $2300..$23FF, $27C0..$27EF, $27F0..$27FF, $2900..$297F, $2980..$29FF,
    $2A00..$2AFF, $1D400..$1D7FF:
      Result := usMathematical;
    $2000..$206F, $20A0..$20CF, $20D0..$20FF, $2400..$243F, $2440..$245F,
    $2460..$24FF, $2500..$257F, $2580..$259F, $25A0..$25FF, $2600..$26FF,
    $2700..$27BF, $2B00..$2BFF:
      Result := usSymbols;
    $3000..$303F, $3300..$33FF, $FE30..$FE4F, $F900..$FAFF:
      Result := usCJKCompatibility;
    $E000..$F8FF, $F0000..$FFFFF, $100000..$10FFFF: Result := usPrivateUse;
    $0300..$036F, $1AB0..$1AFF, $1DC0..$1DFF, $FE20..$FE2F:
      Result := usInherited;
    $11000..$1107F: Result := usBrahmi;
    $10B00..$10B3F: Result := usAvestan;
    $13000..$1342F: Result := usEgyptianHieroglyphs;
    $12000..$123FF: Result := usCuneiform;
    $10000..$1007F: Result := usLinearB;
    $10900..$1091F: Result := usPhoenician;
  else
    Result := usUnknown;
  end;
end;

function ScriptToLang(Script: TUnicodeScript): string;
begin
  Result := ScriptToLangData[Script];
end;

function LangToScript(const ALang: string): TUnicodeScript;
var
  LangLower: string;
  I, Idx: Integer;
begin
  LangLower := LowerCase(ALang);
  if IndexStr(LangLower, LatinLangs) >= 0 then
    Result := usLatin
  else if IndexStr(LangLower, CyrillicLangs) >= 0 then
    Result := usCyrillic
  else if IndexStr(LangLower, HebrewLangs) >= 0 then
    Result := usHebrew
  else if IndexStr(LangLower, ArabicLangs) >= 0 then
    Result := usArabic
  else if IndexStr(LangLower, DevanagariLangs) >= 0 then
    Result := usDevanagari
  else if IndexStr(LangLower, GurmukhiLangs) >= 0 then
    Result := usGurmukhi
  else if IndexStr(LangLower, EthiopicLangs) >= 0 then
    Result := usEthiopic
  else if IndexStr(LangLower, HanLangs) >= 0 then
    Result := usHan
  else if IndexStr(LangLower, TifinaghLangs) >= 0 then
    Result := usTifinagh
  else if IndexStr(LangLower, MathematicalLangs) >= 0 then
    Result := usMathematical
  else
  begin
    Idx := IndexStr(LangLower, SingleLangs);
    case Idx of
      0: Result := usGreek;
      1: Result := usArmenian;
      2: Result := usGeorgian;
      3: Result := usSyriac;
      4: Result := usThaana;
      5: Result := usBengali;
      6: Result := usGujarati;
      7: Result := usOriya;
      8: Result := usTamil;
      9: Result := usTelugu;
      10: Result := usKannada;
      11: Result := usMalayalam;
      12: Result := usSinhala;
      13: Result := usThai;
      14: Result := usLao;
      15: Result := usTibetan;
      16: Result := usMyanmar;
      17: Result := usKhmer;
      18: Result := usCherokee;
      19: Result := usBopomofo;
      20: Result := usBraille;
      21: Result := usEmoji;
    else
      I := IndexStr(LangLower, ScriptToLangData);
      if I >= 0 then
        Result := TUnicodeScript(I)
      else
        Result := usUnknown;
    end;
  end;
end;

{$ENDREGION}

{$REGION ' fontconfig '}

const
  libfontconfig = 'libfontconfig.so.1';

  function FcPatternAddBool(pattern: PFcPattern; const obj: PChar; b: TFcBool): TFcBool; cdecl; external libfontconfig;
  function FcPatternAddDouble(pattern: PFcPattern; const obj: PChar; d: cdouble): TFcBool; cdecl; external libfontconfig;
  function FcPatternAddInteger(pattern: PFcPattern; const obj: PChar; i: cint): TFcBool; cdecl; external libfontconfig;
  function FcPatternAddString(pattern: PFcPattern; obj: PChar; s:PFcChar8): TFcBool; cdecl; external libfontconfig;

  function FcPatternGetBool(const pattern: PFcPattern; const obj: PChar; n: cint; b: PFcBool): TFcResult; cdecl; external libfontconfig;
  function FcPatternGetDouble(const pattern: PFcPattern; const obj: PChar; n: cint; d: pcdouble): TFcResult; cdecl; external libfontconfig;
  function FcPatternGetInteger(const pattern: PFcPattern; const obj: PChar; n: cint; i: pcint): TFcResult; cdecl; external libfontconfig;
  function FcPatternGetString(const pattern: PFcPattern; const obj: PChar; n: cint; s: PFcChar8): TFcResult; cdecl; external libfontconfig;

  function FcPatternAddCharSet(p:PFcPattern; const obj: PChar; c:PFcCharSet): TFcBool; cdecl; external libfontconfig;
  function FcPatternGetLangSet(const pattern:PFcPattern; const obj: PChar; n:cint; ls: PFcLangSet): TFcResult; cdecl; external libfontconfig;

  function FcFreeTypeCharIndex(face: PFT_Face; ucs4: TFcChar32): FT_UInt; cdecl; external libfontconfig;

  function cairo_ft_font_face_create_for_ft_face(face: PFT_Face; load_flags:longint): Pcairo_font_face_t; cdecl; external LIB_CAIRO;

{$ENDREGION}

{$REGION ' Cairo Utilitary '}

var
  CairoColors: array [0..255] of Double;

type
  TCairoColor = record
    R, G, B, A: Double;
  end;

function cairo_create_context(DC: HDC): pcairo_t;
var
  Ctx: TGtkDeviceContext;
  Matrix: cairo_matrix_t;
begin
  Ctx := TGtkDeviceContext(DC);
  Result := gdk_cairo_create(Ctx.Drawable);
  if Result = nil then
    raise ECairoException.Create('Cannot create cairo context');
  if Ctx.WindowExt <> Ctx.ViewPortExt then
  begin
    Matrix.xx := Ctx.ViewPortExt.X / Ctx.WindowExt.X;
    Matrix.yy := Ctx.ViewPortExt.Y / Ctx.WindowExt.Y;
    Matrix.x0 := Ctx.ViewPortOrg.X;
    Matrix.y0 := Ctx.ViewPortOrg.Y;
    Matrix.yx := 0;
    Matrix.xy := 0;
    cairo_set_matrix(Result, @Matrix);
  end;
end;

procedure cairo_set_font_descriptor(ACairo: pcairo_t; AFont: TFont;
  var AFontDescriptor: TFontMapFontDescriptor);
var
  FontMatrix: cairo_matrix_t;
begin
  cairo_set_scaled_font(ACairo, AFontDescriptor.scaled_font);
  if AFont.Height = 0 then
    AFontDescriptor.ff_desc.PixelSize := 13.349609 * 96 / AFont.PixelsPerInch
  else
    AFontDescriptor.ff_desc.PixelSize :=  Abs(AFont.Height);
  cairo_get_font_matrix(ACairo, @FontMatrix);
  FontMatrix.xx := AFontDescriptor.ff_desc.PixelSize;
  FontMatrix.yy := FontMatrix.xx;
  
  // не у всех шрифтов есть Italic, например у Droid Sans отсутствует,
  // поэтому для них используется матрица преобразования.
  // именно поэтому в данном месте нельзя использовать cairo_set_font_size,
  // так как это апи работает только с xx/yy полями матрицы, обнуляя остальные.
  
  // Not all fonts have Italic, for example, Droid Sans does not, so a conversion 
  // matrix is used for them. That is why cairo_set_font_size cannot be used here, 
  // as this API only works with xx/yy matrix fields, resetting the rest to zero.  
  
  if FontMatrix.xy <> 0 then
    FontMatrix.xy := -FontMatrix.xx / 5;
  cairo_set_font_matrix(ACairo, @FontMatrix);
  cairo_scaled_font_extents(cairo_get_scaled_font(ACairo), @AFontDescriptor.extents);
  AFontDescriptor.baseLine := Ceil(AFontDescriptor.extents.ascent);
end;

function cairo_get_color(AColor: TColor): TCairoColor;
begin
  if AColor = clDefault then
    AColor := clBtnText;
  AColor := ColorToRGB(AColor);
  Result.R := CairoColors[GetRValue(AColor)];
  Result.G := CairoColors[GetGValue(AColor)];
  Result.B := CairoColors[GetBValue(AColor)];
  Result.A := 1.0;
end;

procedure cairo_set_source_color(ACairo: pcairo_t; const AColor: TCairoColor);
begin
  cairo_set_source_rgba(ACairo, AColor.R, AColor.G, AColor.B, AColor.A);
end;

procedure ReleaseCairoContext(var AContext: TCairoContext);
begin
  cairo_glyph_free(AContext.Glyphs);
  cairo_text_cluster_free(pcairo_text_cluster_t(AContext.Clusters));
  cairo_destroy(AContext.Context);
end;

function CreateCairoContext(ACanvas: TCanvas; Str: PChar;
  Count, X, Y: Integer; out AContext: TCairoContext): Boolean;
var
  cluster_flags: cairo_text_cluster_flags_t;
  glyphs: Pcairo_glyph_t;
  I: Integer;
  BoldOffset: Double;
  uString: UnicodeString;

begin
  AContext := Default(TCairoContext);
  AContext.Context := cairo_create_context(ACanvas.Handle);
  AContext.Origin.X := X;
  AContext.Origin.Y := Y;
  AContext.Font := CairoFontMap.GetCairoFont(ACanvas.Font);
  cairo_set_font_descriptor(AContext.Context, ACanvas.Font, AContext.Font);
  cairo_set_source_color(AContext.Context, cairo_get_color(ACanvas.Font.Color));

  Result := cairo_scaled_font_text_to_glyphs(cairo_get_scaled_font(AContext.Context),
    X, Y + AContext.Font.baseLine,
    Str, Count, @AContext.Glyphs, @AContext.GlyphsLen, @AContext.Clusters,
    @AContext.ClustersLen, @cluster_flags) = CAIRO_STATUS_SUCCESS;

  if not Result then
  begin
    ReleaseCairoContext(AContext);
    Exit;
  end;

  // если жирный шрифт отсутствует, то используется синтезированный шрифт
  // через вызов FT_GlyphSlot_Embolden который немного увеличивает размер
  // глифов и общая ширина текста увеличивается примерно на 4 процента.
  // Панго корректирует это смещение, и мы будем делать так-же.

  // If bold font is not available, a synthesized font is used via the
  // FT_GlyphSlot_Embolden call, which slightly increases the size of the glyphs
  // and increases the overall width of the text by approximately 4 percent.
  // Pango corrects this offset, and we will do the same.

  if AContext.Font.ff_desc.Embolden = FcTrue then
    BoldOffset := AContext.Font.ff_desc.PixelSize / 24.0
  else
    BoldOffset := 0.0;

  glyphs := AContext.Glyphs;
  uString := UnicodeString(Str);
  for I := 0 to AContext.GlyphsLen - 1 do
  begin
    if glyphs^.index = 0 then
    begin
      AContext.InvalidGlyphsPresent := True;
      if AContext.VisibleGlyphsPresent and (AContext.Font.ff_desc.Embolden = FcFalse) then
        Break;
    end
    else
      if not AContext.VisibleGlyphsPresent and not IsInvisibleChar(TFcChar32(uString[I + 1])) then
      begin
        AContext.VisibleGlyphsPresent := True;
        if AContext.InvalidGlyphsPresent and (AContext.Font.ff_desc.Embolden = FcFalse) then
          Break;
      end;
    if AContext.Font.ff_desc.Embolden = FcTrue then
      glyphs^.x := glyphs^.x - BoldOffset * I;
    Inc(glyphs);
  end;
end;

{$ENDREGION}

{$REGION ' Cairo Draw '}

procedure DrawWithFontFallback(AFont: TFont; const ct: TCairoContext; Str: PChar;
  X: Integer; CalcRect: PRect; Dx: PInteger);
var
  glyphs, start: Pcairo_glyph_t;
  I, A, Len, BytesLen, GlyphOffset, ClusterIdx: Integer;
  CurrentValid, GlyphValid: Boolean;

  {$IFDEF DEBUG_CAIRO_DUMP}
  procedure DebugDumpCharInfo(fd: TFontMapFontDescriptor; ALen: Integer;
    ADef: Boolean; z: Pcairo_glyph_t; zl: Integer);
  var
    AFontFace: TFontFaceDescriptor;
    s: string;
    u: UnicodeString;
    I: Integer;
  begin
    Exit;
    if CalcRect = nil then Exit;
    AFontFace := fd.ff_desc;
    s := StrPas(Str);
    WriteLn;
    WriteLn('Data: "', Copy(s, 1, ALen), '", def = ', BoolToStr(ADef, True));
    Write('  Unicode: ');
    U := s;
    for I := 1 to zl do
      Write(IntToHex(Integer(u[I])), ' ');
    Writeln;
    Write('  Idx: ');
    for I := 1 to zl do
    begin
      Write(IntToStr(z^.index), ' ');
      inc(z);
    end;
    WriteLn;
    WriteLn('  Offset: ', FloatToStr(start^.x));
    WriteLn('  Used Font: ', ExtractFileName(CairoFontMap.FFontFaceList[AFontFace.Index].FontPath));
    WriteLn('  FontFamily: ', CairoFontMap.FFontFaceList[AFontFace.Index].FontFamily);
    WriteLn('  FontStyle: ', CairoFontMap.FFontFaceList[AFontFace.Index].FontStyle);
    WriteLn('  Slant: ', CairoFontMap.FFontFaceList[AFontFace.Index].Slant);
    WriteLn('  Weight: ', CairoFontMap.FFontFaceList[AFontFace.Index].Weight);
    WriteLn('  Width: ', CairoFontMap.FFontFaceList[AFontFace.Index].Width);
    WriteLn('  Embolden: ', AFontFace.Embolden);
  end;
  {$ENDIF}

  procedure DrawPart;
  var
    I: LongInt;
    pY: Double;
    oldFont: Pcairo_scaled_font_t;
    extents: cairo_text_extents_t;
    new_glyphs, new_glyphs_i: Pcairo_glyph_t;
    new_glyphs_cnt: LongInt;
    clusters: PCairoClusterArray;
    clusters_cnt: LongInt;
    cluster_flags: cairo_text_cluster_flags_t;
    fd: TFontMapFontDescriptor;
    FontNotFound: Boolean;
  begin
    if start^.index > 0 then
    begin
      if CalcRect = nil then
      begin
        if Dx <> nil then
        begin
          new_glyphs_i := start;
          for I := 0 to Len - 1 do
          begin
            new_glyphs_i^.x := X;
            Inc(X, Dx^);
            Inc(new_glyphs_i);
            Inc(Dx);
          end;
        end;
        cairo_show_glyphs(ct.Context, start, Len)
      end
      else
      begin
        new_glyphs_i := start + Len - 1;
        cairo_glyph_extents(ct.Context, new_glyphs_i, 1, @extents);
        if ct.VisibleGlyphsPresent then
        begin
          CalcRect^.Width := Ceil(new_glyphs_i^.x + extents.x_advance - ct.Origin.X);
          CalcRect^.Height := Max(Ceil(ct.Font.extents.ascent + ct.Font.extents.descent), CalcRect^.Height);
        end;
      end;
      {$IFDEF DEBUG_CAIRO_DUMP}
      DebugDumpCharInfo(fd, BytesLen, True, start, Len);
      {$ENDIF}
    end
    else
    begin
      oldFont := cairo_get_scaled_font(ct.Context);

      // Этот scaled_font не тот, который хранится в кэше, поэтому
      // чтобы он не разрушился, необходимо временно увеличить счетчик
      // в противном случае придется заново восстанавливать шрифт через
      // дескриптор, ассоциированный с текущим TFont

      // This scaled_font is not the one stored in the cache, so to prevent it
      // from being destroyed, you need to temporarily increase the counter,
      // otherwise you will have to restore the font again through the descriptor
      // associated with the current TFont.

      cairo_scaled_font_reference(oldFont);

      // при автоподстановке могут встретится несколько неизвестных глифов
      // из РАЗНЫХ шрифтов, поэтому вывод автоподстановки делаем в цикле частями

      // When auto-substituting, several unknown glyphs from DIFFERENT fonts may appear,
      // so we perform auto-substitution in a loop in parts.

      while BytesLen > 0 do
      begin
        fd := CairoFontMap.GetCairoCharFont(AFont, Str);
        cairo_set_font_descriptor(ct.Context, AFont, fd);
        new_glyphs := nil;
        new_glyphs_cnt := 0;
        clusters := nil;
        clusters_cnt := 0;

        // Если в основном шрифте отсутствуют видимые символы (из текущей строки),
        // то нужно откорректировать baseline на параметры нового шрифта

        // If there are no visible characters (from the current line) in the
        // main font, you need to adjust the baseline to the parameters of the new font.

        pY := start^.y;
        if not ct.VisibleGlyphsPresent then
          pY := pY - ct.Font.baseline + fd.baseline;

        if cairo_scaled_font_text_to_glyphs(cairo_get_scaled_font(ct.Context),
          start^.x, pY, Str, BytesLen, @new_glyphs, @new_glyphs_cnt, @clusters,
          @clusters_cnt, @cluster_flags) = CAIRO_STATUS_SUCCESS then
        try

          {$IFDEF DEBUG_CAIRO_DUMP}
          if CairoFontMap.FastGlyphSearch then
            DebugDumpCharInfo(fd, BytesLen, False, new_glyphs, new_glyphs_cnt)
          else
            DebugDumpCharInfo(fd, BytesLen, False, new_glyphs, 1);
          {$ENDIF}

          I := 0;
          A := 0;
          new_glyphs_i := new_glyphs;
          FontNotFound := False;
          while A < clusters_cnt do
          begin
            if new_glyphs_i^.index = 0 then
            begin
              if I = 0 then
                FontNotFound := True
              else
                Break;
            end;
            if Dx <> nil then
            begin
              new_glyphs_i^.x := X;
              Inc(X, Dx^);
              Inc(Dx);
            end;
            {$PUSHOPT}{$R-}
            if clusters^[A].num_glyphs = 1 then
            begin
              Dec(BytesLen, clusters^[A].num_bytes);
              Inc(Str, clusters^[A].num_bytes);
              Inc(A);
            end
            else
            begin

              // эта ситуация не протестирована и может встретится только в
              // пользовательском шрифте, поэтому работоспособность не гарантирую

              // This situation has not been tested and may only occur in user
              // fonts, so I cannot guarantee that it will work.

              Dec(clusters^[A].num_glyphs);
              Inc(new_glyphs_i);
              FontNotFound := new_glyphs_i^.index = 0;
              Dec(new_glyphs_i);
              if FontNotFound then
              begin
                Dec(BytesLen, clusters^[A].num_bytes);
                Inc(Str, clusters^[A].num_bytes);
                Break;
              end;
            end;
            {$POPOPT}
            Inc(I);
            Inc(new_glyphs_i);
            if FontNotFound then
              Break;
            if not CairoFontMap.FastGlyphSearch and (I = 1) then
              Break;
          end;
          if I > 0 then
          begin
            Dec(new_glyphs_i);
            cairo_glyph_extents(ct.Context, new_glyphs_i, 1, @extents);
            if CalcRect = nil then
              cairo_show_glyphs(ct.Context, new_glyphs, I)
            else
            begin
              CalcRect^.Width := Ceil(new_glyphs_i^.x + extents.x_advance - ct.Origin.X);
              CalcRect^.Height := Max(Ceil(fd.extents.ascent + fd.extents.descent), CalcRect^.Height);
            end;
            GlyphOffset := Ceil(new_glyphs_i^.x + extents.x_advance - glyphs^.x);
            Inc(start, I);
            if BytesLen > 0 then
              start^.x := new_glyphs_i^.x + extents.x_advance;
          end;
        finally
          cairo_text_cluster_free(pcairo_text_cluster_t(clusters));
          cairo_glyph_free(new_glyphs);
        end;
      end;
      cairo_set_scaled_font(ct.Context, oldFont);
      cairo_scaled_font_destroy(oldFont);
    end;
    start := glyphs;
    Inc(Str, BytesLen);
    BytesLen := 0;
    Len := 0;
  end;

begin
  glyphs := ct.Glyphs;
  start := glyphs;
  BytesLen := 0;
  Len := 0;
  GlyphOffset := 0;
  CurrentValid := glyphs^.index > 0;
  if CalcRect <> nil then
    CalcRect^.Height := 0;
  ClusterIdx := 0;
  for I := 0 to ct.ClustersLen - 1 do
  begin
    {$PUSHOPT}{$R-}
    for A := 0 to ct.Clusters^[I].num_glyphs - 1 do
    begin
      GlyphValid := glyphs^.index > 0;
      if GlyphValid <> CurrentValid then
      begin
        DrawPart;
        ClusterIdx := I;
        CurrentValid := GlyphValid;
      end;
      glyphs^.x := glyphs^.x + GlyphOffset;
      Inc(glyphs);
    end;
    Inc(BytesLen, ct.Clusters^[I].num_bytes);
    Inc(Len);
    {$POPOPT}
  end;
  DrawPart;
end;

function CairoDrawText(ACanvas: TCanvas; const Str: string; var ARect: TRect;
  Flags: Cardinal): Integer;
var
  ct: TCairoContext;
  textents: cairo_text_extents_t;
  glyphs: Pcairo_glyph_t;
  x, y, offset, I: Integer;
  LOrigin: TPoint;
  OriginRect: TRect;
begin
  Result := 0;
  GetWindowOrgEx(ACanvas.Handle, @LOrigin);
  OriginRect := ARect;
  OffsetRect(OriginRect, -LOrigin.X, -LOrigin.Y);

  if not CreateCairoContext(ACanvas, PChar(Str), -1, OriginRect.Left, OriginRect.Top, ct) then Exit;
  try

    Result := Ceil(ct.Font.extents.ascent + ct.Font.extents.descent);

    if Flags and DT_CALCRECT <> 0 then
    begin
      DrawWithFontFallback(ACanvas.Font, ct, PChar(Str), ARect.Left, @ARect, nil);
      Exit;
    end;

    if Flags and DT_NOCLIP = 0 then
    begin
      cairo_rectangle(ct.Context, OriginRect.Left, OriginRect.Top, ARect.Width, ARect.Height);
      cairo_clip(ct.Context);
    end;

    x := OriginRect.Left;
    y := OriginRect.Top + ct.Font.baseLine;
    textents := Default(cairo_text_extents_t);
    if Flags and DT_CENTER <> 0 then
    begin
      cairo_text_extents(ct.Context, PChar(Str), @textents);
      x := OriginRect.Left + (ARect.Width - Ceil(textents.width)) div 2;
    end;
    if Flags and DT_RIGHT <> 0 then
    begin
      cairo_text_extents(ct.Context, PChar(Str), @textents);
      x := OriginRect.Right - Ceil(textents.width);
    end;

    offset := x - OriginRect.Left;
    if offset <> 0 then
    begin
      glyphs := ct.Glyphs;
      for I := 0 to ct.GlyphsLen - 1 do
      begin
        glyphs^.x := glyphs^.x + offset;
        Inc(glyphs);
      end;
    end;

    if ACanvas.Brush.Style = bsSolid then
    begin
      OriginRect.Left := Max(X, OriginRect.Left);
      OffsetRect(OriginRect, LOrigin.X, LOrigin.Y);
      ACanvas.FillRect(OriginRect);
    end;

    if ct.InvalidGlyphsPresent then
      DrawWithFontFallback(ACanvas.Font, ct, PChar(Str), x, nil, nil)
    else
      cairo_show_glyphs(ct.Context, ct.Glyphs, ct.GlyphsLen);

  finally
    ReleaseCairoContext(ct);
  end;
end;

function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
  ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ct: TCairoContext;
  glyphs: Pcairo_glyph_t;
  I, A: Integer;
  LOrigin: TPoint;
begin
  GetWindowOrgEx(ACanvas.Handle, @LOrigin);
  Dec(X, LOrigin.X);
  Dec(Y, LOrigin.Y);
  Result := CreateCairoContext(ACanvas, Str, Count, X, Y, ct);
  if not Result then Exit;
  try
    if ARect <> nil then
    begin
      if ACanvas.Brush.Style = bsSolid then
        ACanvas.FillRect(ARect^);
      if Options and ETO_CLIPPED <> 0 then
      begin
        cairo_rectangle(ct.Context, ARect^.Left, ARect^.Top, ARect^.Width + 1, ARect^.Height);
        cairo_clip(ct.Context)
      end;
    end;

    if ct.InvalidGlyphsPresent then
    begin
      DrawWithFontFallback(ACanvas.Font, ct, PChar(Str), X, nil, Dx);
      Exit;
    end;

    if Dx <> nil then
    begin
      glyphs := ct.Glyphs;
      for I := 0 to ct.GlyphsLen - 1 do
      begin
        glyphs^.x := X;
        Inc(X, Dx^);
        Inc(glyphs);
        Inc(Dx);
      end;
    end;

    cairo_show_glyphs(ct.Context, ct.Glyphs, ct.GlyphsLen);

  finally
    ReleaseCairoContext(ct);
  end;
end;

{$ENDREGION}

{$REGION ' TFontMapFontInfoComparer '}

{ TFontMapFontInfoComparer }

function TFontMapFontInfoComparer.Equals(const Left, Right: TFontMapFontInfo
  ): Boolean;
begin
  Result :=
    AnsiSameText(Left.Name, Right.Name) and
    (Left.Bold = Right.Bold) and
    (Left.Italic = Right.Italic) and
    (Left.Script = Right.Script) and
    (Left.StrikeOut = Right.StrikeOut) and
    (Left.Underline = Right.Underline);
end;

function TFontMapFontInfoComparer.GetHashCode(const Value: TFontMapFontInfo
  ): LongWord;
var
  I: Integer;
begin
  {$PUSHOPT}{$R-}{$Q-}
  Result := 2166136261 * mORMotHasher(0, @Value.Name[1], Length(Value.Name));
  Result := (Result xor LongWord(
    Byte(Value.Bold) +
    Byte(Value.Italic) shl 1 +
    Byte(Value.StrikeOut) shl 2 +
    Byte(Value.Underline) shl 3)) * 16777619;
  Result := (Result xor Byte(Value.Script)) * 16777619
  {$POPOPT}
end;

{$ENDREGION}

{$REGION ' TCairoFontMap '}

{ TCairoFontMap }

class constructor TCairoFontMap.CairoFontMapCreate;
begin
  FInstance := TCairoFontMap.Create;
end;

class destructor TCairoFontMap.CairoFontMapDestroy;
begin
  FreeAndNil(FInstance);
end;

constructor TCairoFontMap.Create;
var
  Comparer: IEqualityComparer<TFontMapFontInfo>;
begin
  FFastGlyphSearch := True;
  FFontFaceFamily := TObjectDictionary<string, TListOfInteger>.Create([doOwnsValues]);
  FFontPathList := TStringList.Create;
  FLoadedFontList := TList<TFontMapFontDescriptor>.Create;
  Comparer := TFontMapFontInfoComparer.Create;
  FLoadedFonts := TDictionary<TFontMapFontInfo, Integer>.Create(Comparer);
  FLoadedFontHandles := TDictionary<Pcairo_scaled_font_t, Integer>.Create;
  if FT_Init_FreeType(FLibrary) <> 0 then
    raise ECairoException.Create('FT_Init_FreeType faied');
  LoadFontConfigLib('');
  InitFonts;
end;

function TCairoFontMap.CreateFontDescriptor(AFont: TFont;
  const AFontFace: TFontFaceDescriptor): TFontMapFontDescriptor;
var
  FaceIndex: Integer;
  ft_font_face: PFT_Face;
  cairo_font_face: Pcairo_font_face_t;
  FontMatrix, CTM: cairo_matrix_t;
  Options: Pcairo_font_options_t;
  extents: cairo_font_extents_t;
  ft_face_key: cairo_user_data_key_t;
  {$IFDEF DEBUG_CAIRO_DUMP}
  TmpFontFace: TFontFaceDescriptor;
  {$ENDIF}
begin
  Result := Default(TFontMapFontDescriptor);

  Result.scaled_font := FFontFaceList[AFontFace.Index].FontHandles[AFontFace.FontType];
  if Result.scaled_font <> nil then
  begin
    Result.ff_desc := AFontFace;
    Exit;
  end;

  ft_font_face := GetFT_Face(AFontFace);
  cairo_font_face := cairo_ft_font_face_create_for_ft_face(ft_font_face, 0);
  cairo_matrix_init_identity(@CTM);
  cairo_matrix_init_scale(@FontMatrix, AFontFace.PixelSize, AFontFace.PixelSize);
  Options := cairo_font_options_create;
  cairo_font_options_set_hint_style(Options, CAIRO_HINT_STYLE_SLIGHT);
  cairo_font_options_set_hint_metrics(Options, CAIRO_HINT_METRICS_ON);
  cairo_font_options_set_antialias(Options, CAIRO_ANTIALIAS_SUBPIXEL);
  cairo_font_options_set_subpixel_order(Options, CAIRO_SUBPIXEL_ORDER_RGB);

  // если не смогли подобрать Italic шрифт, то для эмуляции оного
  // (по аналогии с панго) выполняем преобразование через матрицу.

  // If you couldn't find an Italic font, then to emulate it (similar to pango),
  // perform a conversion using a matrix.

  if (fsItalic in AFont.Style) and not (FFontFaceList[AFontFace.Index].Slant in [FC_SLANT_ITALIC, FC_SLANT_OBLIQUE]) then
    FontMatrix.xy := -FontMatrix.xx / 5;

  // для синтезированных Bold шрифтов необходимо включить флаг

  // for synthesized Bold fonts, you need to enable the flag.

  if AFontFace.Embolden = FcTrue then
    cairo_ft_font_face_set_synthesize(cairo_font_face, LongWord(CAIRO_FT_SYNTHESIZE_BOLD));

  Result.scaled_font := cairo_scaled_font_create(cairo_font_face, @FontMatrix, @CTM, Options);

  cairo_font_options_destroy(Options);
  cairo_font_face_destroy(cairo_font_face);

  {$IFDEF DEBUG_CAIRO_DUMP}
  if FLoadedFontHandles.TryGetValue(Result.scaled_font, FaceIndex) then
  begin
    TmpFontFace := FLoadedFontList[FaceIndex].ff_desc;
    WriteLn('Load: ', FFontFaceList[TmpFontFace.Index].FontPath);
    WriteLn('  FontFamily: ', FFontFaceList[TmpFontFace.Index].FontFamily);
    WriteLn('  FontStyle: ', FFontFaceList[TmpFontFace.Index].FontStyle);
    WriteLn('  Slant: ', FFontFaceList[TmpFontFace.Index].Slant);
    WriteLn('  Weight: ', FFontFaceList[TmpFontFace.Index].Weight);
    WriteLn('  Width: ', FFontFaceList[TmpFontFace.Index].Width);
    WriteLn('  Matrix.xy: ', FontMatrix.xy);
    WriteLn('  Embolden: ', TmpFontFace.Embolden);
  end;
  {$ENDIF}

  if Result.scaled_font = nil then
    raise ECairoException.Create('cairo_scaled_font_create failed.')
  else
  begin
    Result.ff_desc := AFontFace;
    FFontFaceList[AFontFace.Index].FontHandles[AFontFace.FontType] := Result.scaled_font;
  end;
end;

procedure TCairoFontMap.ReleaseFontFaceList;
var
  I, RefCnt: Integer;
  F: TCairoScaledFontType;
begin
  for I := 0 to Length(FFontFaceList) - 1 do
  begin
    for F := Low(TCairoScaledFontType) to High(TCairoScaledFontType) do
    begin
      if FFontFaceList[I].FontHandles[F] = nil then
        Continue;
      RefCnt := cairo_scaled_font_get_reference_count(FFontFaceList[I].FontHandles[F]);
      {$IFDEF DEBUG_CAIRO_DUMP}
      if RefCnt <> 1 then
      begin
        Writeln('Invalid reference count (', RefCnt, ') in "',
          FFontFaceList[I].FontPath, '", font handle: ', IntToHex(UInt64(FFontFaceList[I].FontHandles[F])));
      end;
      {$ENDIF}
      if RefCnt > 0 then
      begin
        cairo_scaled_font_destroy(FFontFaceList[I].FontHandles[F]);
        FFontFaceList[I].FontHandles[F] := nil;
      end;
    end;
    if FFontFaceList[I].FaceHandles[False] <> nil then
    begin
      FT_Done_Face(FFontFaceList[I].FaceHandles[False]);
      FFontFaceList[I].FaceHandles[False] := nil;
    end;
    if FFontFaceList[I].FaceHandles[True] <> nil then
    begin
      FT_Done_Face(FFontFaceList[I].FaceHandles[True]);
      FFontFaceList[I].FaceHandles[True] := nil;
    end;
  end;
end;

procedure TCairoFontMap.SetFastGlyphSearch(AValue: Boolean);
begin
  if FFastGlyphSearch <> AValue then
  begin
    FFastGlyphSearch := AValue;
    ClearCache;
  end;
end;

destructor TCairoFontMap.Destroy;
var
  Descriptor: TFontMapFontDescriptor;
begin
  for Descriptor in FLoadedFontList do
    Descriptor.cache.Free;
  FFontFaceFamily.Free;
  FFontPathList.Free;
  FLoadedFontList.Free;
  FLoadedFontHandles.Free;
  FLoadedFonts.Free;
  ReleaseFontFaceList;
  FT_Done_FreeType(FLibrary);
  UnLoadFontConfigLib;
  inherited Destroy;
end;

procedure TCairoFontMap.ClearCache;
var
  Descriptor: TFontMapFontDescriptor;
  I: Integer;
begin
  for I := 0 to FLoadedFontList.Count - 1 do
  begin
    Descriptor := FLoadedFontList[I];
    if Descriptor.cache <> nil then
    begin
      FreeAndNil(Descriptor.cache);
      FLoadedFontList[I] := Descriptor;
    end;
  end;
  FLoadedFontList.Clear;
  FLoadedFontHandles.Clear;
  FLoadedFonts.Clear;
end;

function TCairoFontMap.CollectStat: TCairoFontMapStat;
type
  TFontFacePair = TPair<string, TListOfInteger>;
var
  List: TArray<TFontFacePair>;
  Item: TFontFacePair;
  I, Idx: Integer;
  F: TCairoScaledFontType;
begin
  Result := Default(TCairoFontMapStat);
  Result.KnownFontCount := FFontPathList.Count;
  Result.AssotiatedFontInfoCount := FLoadedFonts.Count;
  Result.LoadedFontDescriptorCount := FLoadedFontList.Count;
  Result.TotalFontHandles := FLoadedFontHandles.Count;
  List := FFontFaceFamily.ToArray;
  SetLength(Result.FontFamilyStat, Length(List));
  Idx := 0;
  for Item in List do
  begin
    Result.FontFamilyStat[Idx].Name := Item.Key;
    Result.FontFamilyStat[Idx].FontsCount := Item.Value.Count;
    for I in Item.Value do
    begin
      if FFontFaceList[I].FaceHandles[False] <> nil then
        Inc(Result.FontFamilyStat[Idx].LoadedFontFaceCount);
      if FFontFaceList[I].FaceHandles[True] <> nil then
        Inc(Result.FontFamilyStat[Idx].LoadedFontFaceCount);
      for F := Low(TCairoScaledFontType) to High(TCairoScaledFontType) do
        if FFontFaceList[I].FontHandles[F] <> nil then
          Inc(Result.FontFamilyStat[Idx].LoadedFontsCount);
    end;
    Inc(Idx);
  end;
end;

procedure TCairoFontMap.ExtractFontParam(AFont: TFont; out AFace,
  AStyle: string; out ASlant, AWeight, AWidth: Integer);
var
  Idx, SpaceIdx: Integer;
  TmpStyle: string;
begin
  AFace := AFont.Name;
  AWeight := -1;
  AWidth := 0;
  AStyle := '';

  SpaceIdx := AFace.LastDelimiter(' ');
  if SpaceIdx > 0 then
  begin
    TmpStyle := LowerCase(Copy(AFace, SpaceIdx + 2, Length(AFace)));
    Idx := IndexStr(TmpStyle, FontWeights);
    case Idx of
      0: AWeight := FC_WEIGHT_THIN;
      1, 2: AWeight := FC_WEIGHT_EXTRALIGHT;
      3: AWeight := FC_WEIGHT_LIGHT;
      4, 5: AWeight := FC_WEIGHT_SEMILIGHT;
      6: AWeight := FC_WEIGHT_BOOK;
      7, 8: ; // для Regular и Normal ориентируемся на Bold / For Regular and Normal, we focus on Bold.
      9: AWeight := FC_WEIGHT_MEDIUM;
      10, 11: AWeight := FC_WEIGHT_SEMIBOLD;
      12: AWeight := FC_WEIGHT_BOLD;
      13, 14: AWeight := FC_WEIGHT_ULTRABOLD;
      15, 16: AWeight := FC_WEIGHT_HEAVY;
      17, 18: AWeight := FC_WEIGHT_ULTRABLACK;
    end;
    if Idx >= 0 then
    begin
      AStyle := Copy(AFace, SpaceIdx + 1, Length(AFace));
      SetLength(AFace, SpaceIdx);
    end;
  end;

  SpaceIdx := AFace.LastDelimiter(' ');
  if SpaceIdx > 0 then
  begin
    TmpStyle := LowerCase(Copy(AFace, SpaceIdx + 2, Length(AFace)));
    Idx := IndexStr(TmpStyle, FontWidths);
    case Idx of
      0: AWidth := FC_WIDTH_ULTRACONDENSED;
      1: AWidth := FC_WIDTH_EXTRACONDENSED;
      2: AWidth := FC_WIDTH_CONDENSED;
      3: AWidth := FC_WIDTH_SEMICONDENSED;
      4: AWidth := FC_WIDTH_NORMAL;
      5: AWidth := FC_WIDTH_SEMIEXPANDED;
      6: AWidth := FC_WIDTH_EXPANDED;
      7: AWidth := FC_WIDTH_EXTRAEXPANDED;
      8: AWidth := FC_WIDTH_ULTRAEXPANDED;
    end;
    if Idx >= 0 then
    begin
      AStyle := Copy(AFace, SpaceIdx + 1,
        Length(AFace)) + AStyle;
      SetLength(AFace, SpaceIdx);
    end;
  end;

  AStyle := Trim(AStyle);

  if AWeight < 0 then
    AWeight := IfThen(fsBold in AFont.Style, FC_WEIGHT_BOLD, FC_WEIGHT_REGULAR);

  if AWidth = 0 then
    AWidth := FC_WIDTH_NORMAL;

  if fsItalic in AFont.Style then
    ASlant := FC_SLANT_ITALIC
  else
    ASlant := FC_SLANT_ROMAN;
end;

function TCairoFontMap.GetFontFaceIndex(const FontPath: string): Integer;
begin
  Result := FFontPathList.IndexOf(FontPath);
  if Result >= 0 then
    Result := Integer(FFontPathList.Objects[Result]);
end;

procedure TCairoFontMap.InitFonts;
var
  FontSet: PFcFontSet;
  SearchPattern, FoundPattern: PFcPattern;
  I: Integer;
  AFontFile, AFontFamily, AFontStyle, ATmp: PFcChar8;
  AList: TListOfInteger;
  LangSet: PFcLangSet;
  LangStr: PFcStrSet;
  Lang: PFcChar8;
  StrList: PFcStrList;
  Count: Integer;
begin
  SearchPattern := FcPatternCreate;
  try
    FontSet := FcFontList(FcConfigGetCurrent, SearchPattern, nil);
    if FontSet <> nil then
    try
      SetLength(FFontFaceList, FontSet^.nfont);
      for I := 0 to FontSet^.nfont - 1 do
      begin
        AFontFile := nil;
        FoundPattern := FontSet^.fonts[I];
        if FcPatternGetString(FoundPattern, FC_FILE, 0, @AFontFile) = FcResultMatch then
        begin
          with FFontFaceList[I] do
          begin
            FcPatternGetInteger(FoundPattern, FC_SLANT, 0, @Slant);
            FcPatternGetInteger(FoundPattern, FC_WEIGHT, 0, @Weight);
            FcPatternGetInteger(FoundPattern, FC_WIDTH, 0, @Width);
            AFontFamily := nil;
            FcPatternGetString(FoundPattern, FC_FAMILY, 0, @AFontFamily);
            AFontStyle := nil;
            FcPatternGetString(FoundPattern, FC_STYLE, 0, @AFontStyle);
            FontFamily := StrPas(AFontFamily);
            FontPath := StrPas(AFontFile);
            FontStyle := StrPas(AFontStyle);

            if FcPatternGetLangSet(FoundPattern, FC_LANG, 0, @LangSet) = FcResultMatch then
            begin
              LangStr := FcLangSetGetLangs(LangSet);

              if Assigned(LangStr) then
              begin
                StrList := FcStrListCreate(LangStr);
                if Assigned(StrList) then
                begin
                  try
                    repeat
                      Lang := FcStrListNext(StrList);
                      if Assigned(Lang) then
                        Include(Scripts, LangToScript(string(PChar(Lang))));
                    until not Assigned(Lang);

                  finally
                    FcStrListDone(StrList);
                  end;
                end;

                FcStrSetDestroy(LangStr);
              end;
            end;

            FFontPathList.AddObject(FontPath, Pointer(I));
            if not FFontFaceFamily.TryGetValue(FontFamily, AList) then
            begin
              AList := TListOfInteger.Create;
              FFontFaceFamily.Add(FontFamily, AList);
            end;
            AList.Add(I);

          end;
        end;
      end;
    finally
      FcFontSetDestroy(FontSet);
    end;
  finally
    FcPatternDestroy(SearchPattern);
  end;
  FFontPathList.Sorted := True;
end;

function TCairoFontMap.FindMatchFont(AFont: TFont; UnicodeChar: TFcChar32;
  out AFontFaceDescriptor: TFontFaceDescriptor): Boolean;
var
  Config: PFcConfig;
  Pattern, MatchedPattern: PFcPattern;
  Script: TUnicodeScript;
  CharSet: PFcCharSet;
  QueryWeight, QuerySlant, QueryWidth, FaceIndex: Integer;
  FcRes: TFcResult;
  FontFile: PFcChar8;
  QueryFontFace, QueryFontStyle, FontPath: string;
  I: Integer;
  AFamilyList: TListOfInteger;
begin
  Result := False;
  Script := usLatin;
  AFontFaceDescriptor := Default(TFontFaceDescriptor);
  AFontFaceDescriptor.PixelSize := 18.0;
  Config := FcConfigGetCurrent;
  Pattern := FcPatternCreate;
  try
    ExtractFontParam(AFont, QueryFontFace, QueryFontStyle, QuerySlant, QueryWeight, QueryWidth);
    FcPatternAddString(Pattern, FC_FAMILY, PChar(QueryFontFace));
    FcPatternAddInteger(Pattern, FC_WEIGHT, QueryWeight);
    FcPatternAddInteger(Pattern, FC_WIDTH, QueryWidth);
    FcPatternAddInteger(Pattern, FC_SLANT, QuerySlant);
    FcPatternAddBool(Pattern, FC_SCALABLE, 1);
    FcPatternAddBool(Pattern, FC_OUTLINE, 1);
    if UnicodeChar <> 0 then
    begin
      Script := CharToScript(UnicodeChar);
      if Script <> usUnknown then
        FcPatternAddString(Pattern, FC_LANG, PChar(ScriptToLang(Script)));
      CharSet := FcCharSetCreate;
      FcCharSetAddChar(CharSet, UnicodeChar);
      FcPatternAddCharSet(Pattern, FC_CHARSET, CharSet);
      FcCharSetDestroy(CharSet);
    end;
    FcConfigSubstitute(Config, Pattern, FcMatchPattern);
    FcDefaultSubstitute(Pattern);

    FcRes := FcResultNoMatch;
    MatchedPattern := FcFontMatch(Config, Pattern, @FcRes);
    if MatchedPattern <> nil then
    try
      if FcRes = FcResultMatch then
      begin
        FontFile := nil;
        if FcPatternGetString(MatchedPattern, FC_FILE, 0, @FontFile) = FcResultMatch then
        begin
          Result := True;

          // Флаг для синтезированного Bold шрифта

          // Flag for synthesized Bold font

          if FcPatternGetBool(MatchedPattern, FC_EMBOLDEN, 0, @AFontFaceDescriptor.Embolden) <> FcResultMatch then
            AFontFaceDescriptor.Embolden := FcFalse;

          AFontFaceDescriptor.Index := GetFontFaceIndex(StrPas(FontFile));

          AFontFaceDescriptor.FontType := TCairoScaledFontType(Byte(AFont.Bold) or (Byte(AFont.Italic) shl 1));

          if AFontFaceDescriptor.Index < 0 then
            raise ECairoException.Create('Unknown font: ' + StrPas(FontFile));

          if FcPatternGetInteger(MatchedPattern, FC_INDEX, 0, @FaceIndex) = FcResultMatch then
            FFontFaceList[AFontFaceDescriptor.Index].FaceIndex := FaceIndex;

        end;
      end;
    finally
      FcPatternDestroy(MatchedPattern);
    end;
  finally
    FcPatternDestroy(Pattern);
  end;
end;

function TCairoFontMap.GetFT_Face(const AFontFace: TFontFaceDescriptor): PFT_Face;
begin
  // для Regular и Italic шрифтов (с использованием матрицы) достаточно
  // обычного FontFace, потому что при проверке, не является ли новый шрифт
  // текущим (_cairo_scaled_font_matches(font_map->mru_scaled_font))
  // используется как FontFace, так и матрица, применяемая для синтезации Italic.
  // А вот для некоторых жирных шрифтов (в том числе для синтезируемых, у которых
  // выставляется флаг CAIRO_FT_SYNTHESIZE_BOLD), поле ft_options.synth_flags
  // не учавствует в проверке, и при попытке создания такого шрифта,
  // вернется ранее созданый шрифт для FontFace = Regular с увеличеным ref_count
  // (если таковой присутствует).
  // Поэтому чтобы избежать данной проблемы, все Bold шрифты создаются на
  // отдельном FontFace

  // For Regular and Italic fonts (using a matrix), a regular FontFace is
  // sufficient because when checking whether a new font is the current one
  // (_cairo_scaled_font_matches(font_map->mru_scaled_font)), both FontFace and
  // the matrix used for Italic synthesis are used. However, for some bold fonts
  // (including synthesized ones with the CAIRO_FT_SYNTHESIZE_BOLD flag set),
  // the ft_options.synth_flags field is not involved in the check, and when
  // attempting to create such a font, the previously created font
  // for FontFace = Regular will be returned with an increased ref_count (if present).
  // Therefore, to avoid this problem, all Bold fonts are created on a separate FontFace.

  Result := FFontFaceList[AFontFace.Index].FaceHandles[AFontFace.FontType in [csftBold, csftBoldItalic]];
  if Result = nil then
  begin
    with FFontFaceList[AFontFace.Index] do
    begin
      if FT_New_Face(FLibrary, @FontPath[1], FaceIndex, Result) <> 0 then
        raise ECairoException.CreateFmt('FT_New_Face(%s) failed.', [FontPath]);
      FaceHandles[AFontFace.FontType in [csftBold, csftBoldItalic]] := Result;
    end;
  end;
end;

function TCairoFontMap.GetFontInfo(AFont: TFont): TFontMapFontInfo;
begin
  Result := Default(TFontMapFontInfo);
  Result.Name := AFont.Name;
  Result.Bold := fsBold in AFont.Style;
  Result.Italic := fsItalic in AFont.Style;
  Result.Script := usLatin;
end;

function TCairoFontMap.GetCairoCharFont(AFont: TFont; Str: PChar): TFontMapFontDescriptor;
var
  FontInfo: TFontMapFontInfo;
  FontIndex: Integer;
  RootFont: TFontMapFontDescriptor;
  AUChar: UnicodeChar;
begin
  FontInfo := GetFontInfo(AFont);
  if FLoadedFonts.TryGetValue(FontInfo, FontIndex) then
    RootFont := FLoadedFontList[FontIndex]
  else
  begin
    {$IFDEF UNIT_TEST}

    // это не штатное использование и сделано только для юнит тестирования!!!

    // This is not intended for regular use and is only for unit testing purposes!!!

    RootFont := GetCairoFont(AFont);
    if not FLoadedFonts.TryGetValue(FontInfo, FontIndex) then
      FontIndex := FLoadedFontList.Add(RootFont);
    {$ELSE}
    raise ECairoException.Create('No root font!');
    {$ENDIF}
  end;

  // GetCairoCharFont должен вызываться только для символов,
  // отсутствующих в шрифте, назначеном по умолчанию для текущего контекста.
  // Поэтому кэш автоподстановки подключается именно к основному шрифту.

  // GetCairoCharFont should only be called for characters
  // that are not present in the default font for the current context.
  // Therefore, the autofill cache is connected to the base font.

  if RootFont.cache = nil then
  begin
    RootFont.cache := TDictionary<TFcChar32, Integer>.Create;
    FLoadedFontList[FontIndex] := RootFont;
  end;

  Utf8ToUnicode(@AUChar, Str, 1);
  if RootFont.cache.TryGetValue(TFcChar32(AUChar), FontIndex) then
    Exit(FLoadedFontList[FontIndex]);

  if not FindMatchFont(AFont, TFcChar32(AUChar), Result.ff_desc) then
    raise ECairoException.CreateFmt('Font "%s [%s]" not found.', [AFont.Name, ScriptToLang(FontInfo.Script)]);

  Result := CreateFontDescriptor(AFont, Result.ff_desc);

  if not FLoadedFontHandles.TryGetValue(Result.scaled_font, FontIndex) then
  begin
    FontIndex := FLoadedFontList.Add(Result);
    FLoadedFontHandles.Add(Result.scaled_font, FontIndex);
  end;

  RootFont.cache.Add(TFcChar32(AUChar), FontIndex);
end;

function TCairoFontMap.GetCairoFont(AFont: TFont): TFontMapFontDescriptor;
var
  FontInfo: TFontMapFontInfo;
  FontIndex: Integer;
begin
  FontInfo := GetFontInfo(AFont);
  if FLoadedFonts.TryGetValue(FontInfo, FontIndex) then
    Result := FLoadedFontList[FontIndex]
  else
  begin
    Result := Default(TFontMapFontDescriptor);
    if not FindMatchFont(AFont, 0, Result.ff_desc) then
      raise ECairoException.CreateFmt('Font "%s" not found.', [AFont.Name]);
    Result := CreateFontDescriptor(AFont, Result.ff_desc);
    if not FLoadedFontHandles.TryGetValue(Result.scaled_font, FontIndex) then
    begin
      FontIndex := FLoadedFontList.Add(Result);
      FLoadedFontHandles.Add(Result.scaled_font, FontIndex);
    end;
    FLoadedFonts.Add(FontInfo, FontIndex);
  end;
end;

function CairoFontMap: TCairoFontMap;
begin
  Result := TCairoFontMap.FInstance;
end;

{$ENDREGION}

procedure InitCairoColors;
var
  I: Integer;
begin
  for I := 0 to 255 do
    CairoColors[I] := I / 255;
end;

initialization

  InitCairoColors;

end.

