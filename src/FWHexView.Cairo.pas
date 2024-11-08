////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Cairo.pas
//  * Purpose   : Speeding up text output with the Cairo library
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.14
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//

{
Licence:
  FWHexView is dual-licensed. You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence. A commercial licence grants you the right to use FWHexView in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FWHexView
  to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge. A commercial licence is sold per developer developing applications that use FWHexView, as follows:
    1 developer = $49
    2 developers = $89
    3 developers = $139
    4 developers = $169
    5 developers = $199
    >5 developers = $199 + $25 per developer from the 6th onwards
    site licence = $499 (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)

  Please send an e-mail to rouse79@yandex.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.Cairo;

{$mode Delphi}{$H+}

interface

uses
  LCLType,
  LCLIntf,
  Types,
  SysUtils,
  Graphics;

  function CairoDrawText(ACanvas: TCanvas; Str: string;
    var ARect: TRect; Flags: Cardinal): Integer;
  function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

implementation

uses
  gdk2,
  Gtk2Def,
  Cairo,
  Math;

type
  ECairoException = class(Exception);

  TCairoColor = record
    R, G, B, A: Double;
  end;

  PCairoClusterArray = ^TCairoClusterArray;
  TCairoClusterArray = array[0..0] of cairo_text_cluster_t;

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

procedure cairo_set_font(ACairo: pcairo_t; AFont: TFont);
var
  ADefFont: TFontData;
  LSlant: cairo_font_slant_t;
  LWeight: cairo_font_weight_t;
begin
  if fsItalic in AFont.Style then
    LSlant := CAIRO_FONT_SLANT_ITALIC
  else
    LSlant := CAIRO_FONT_SLANT_NORMAL;

  if fsBold in AFont.Style then
    LWeight := CAIRO_FONT_WEIGHT_BOLD
  else
    LWeight := CAIRO_FONT_WEIGHT_NORMAL;

  ADefFont := GetFontData(GetStockObject(DEFAULT_GUI_FONT));

  if AFont.IsDefault then
    cairo_select_font_face(ACairo, PChar(string(ADefFont.Name)), LSlant, LWeight)
  else
    cairo_select_font_face(ACairo, PChar(AFont.Name), LSlant, LWeight);

  if AFont.Height = 0 then
    cairo_set_font_size(ACairo, ADefFont.Height)
  else
    cairo_set_font_size(ACairo, Abs(AFont.Height));
end;

function cairo_get_color(AColor: TColor): TCairoColor;
begin
  AColor := ColorToRGB(AColor);
  Result.R := GetRValue(AColor) / 255;
  Result.G := GetGValue(AColor) / 255;
  Result.B := GetBValue(AColor) / 255;
  Result.A := 1.0;
end;

procedure cairo_set_source_color(ACairo: pcairo_t; const AColor: TCairoColor);
begin
  cairo_set_source_rgba(ACairo, AColor.R, AColor.G, AColor.B, AColor.A);
end;

function cairo_font_baseline(AFont: Pcairo_scaled_font_t): Integer;
var
  extents: cairo_font_extents_t;
begin
  cairo_scaled_font_extents(AFont, @extents);
  Result := Ceil(extents.height - extents.descent);
end;

function CairoDrawText(ACanvas: TCanvas; Str: string;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  ct: pcairo_t;
  sfont: Pcairo_scaled_font_t;
  fextents: cairo_font_extents_t;
  textents: cairo_text_extents_t;
  x, y, awidth: Integer;
begin
  Result := 0;
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_set_font(ct, ACanvas.Font);
    cairo_set_source_color(ct, cairo_get_color(ACanvas.Font.Color));
    sfont := cairo_get_scaled_font(ct);
    cairo_scaled_font_extents(sfont, @fextents);
    Result := Ceil(fextents.height);
    x := ARect.Left;
    y := ARect.Top + cairo_font_baseline(sfont);
    if Flags and DT_NOCLIP = 0 then
    begin
      cairo_rectangle(ct, ARect.Left, ARect.Top, ARect.Width + 1, ARect.Height);
      cairo_clip(ct);
    end;
    cairo_text_extents(ct, PChar(Str), @textents);
    awidth := Ceil(textents.width);
    if Flags and DT_CENTER <> 0 then
      x := ARect.Left + (ARect.Width - awidth) div 2;
    if Flags and DT_RIGHT <> 0 then
      x := ARect.Right - awidth;
    if ACanvas.Brush.Style = bsSolid then
    begin
      ARect.Left := X;
      ARect.Width := awidth + 1;
      ACanvas.FillRect(ARect);
    end;
    cairo_move_to(ct, x, y);
    cairo_show_text(ct, PChar(Str));
  finally
    cairo_destroy(ct);
  end;
end;

function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
  ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ct: pcairo_t;
  AFont: Pcairo_scaled_font_t;
  glyph, glyphs: Pcairo_glyph_t;
  num_glyphs: LongInt;
  clusters: PCairoClusterArray;
  num_clusters: LongInt;
  cluster_flags: cairo_text_cluster_flags_t;
  I, A: Integer;
begin
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_set_font(ct, ACanvas.Font);
    AFont := cairo_get_scaled_font(ct);
    num_glyphs := 0;
    num_clusters := 0;
    clusters := nil;
    glyphs := nil;
    Result := cairo_scaled_font_text_to_glyphs(AFont, X, Y + cairo_font_baseline(AFont),
      Str, Count, @glyphs, @num_glyphs, @clusters, @num_clusters, @cluster_flags) = CAIRO_STATUS_SUCCESS;
    if not Result then Exit;
    if Dx <> nil then
    begin
      glyph := glyphs;
      for I := 0 to num_clusters - 1 do
        for A := 0 to clusters^[I].num_glyphs - 1 do
        begin
          glyph^.x := X;
          Inc(X, Dx^);
          Inc(glyph);
          Inc(Dx);
        end;
    end;
    if ARect <> nil then
    begin
      cairo_rectangle(ct, ARect^.Left, ARect^.Top, ARect^.Width + 1, ARect^.Height);
      if ACanvas.Brush.Style = bsSolid then
      begin
        ARect^.Right := X + 2;
        ACanvas.FillRect(ARect^);
      end;
      if Options and ETO_CLIPPED <> 0 then
        cairo_clip(ct);
    end;
    cairo_set_source_color(ct, cairo_get_color(ACanvas.Font.Color));
    cairo_glyph_path(ct, glyphs, num_glyphs);
    cairo_fill_preserve(ct);
    cairo_glyph_free(glyphs);
    cairo_text_cluster_free(pcairo_text_cluster_t(clusters));
  finally
    cairo_destroy(ct);
  end;
end;

end.

