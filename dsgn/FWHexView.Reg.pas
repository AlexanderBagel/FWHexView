////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Reg.pas
//  * Purpose   : Registration module for FWHexView
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

unit FWHexView.Reg;

interface

{TODO: подцепить картинки}
//{$R .\img.res}

uses
  {$IFDEF FPC}
  ActnList,
  LResources,
  {$ELSE}
  Actions,
  DesignIntf,
  DesignEditors,
  {$ENDIF}
  Classes,
  FWHexView,
  FWHexView.MappedView,
  FWHexView.Actions;

{$IFNDEF FPC}
type
  TFWHexViewActionsSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC}
  ForceDemandLoadState(dlDisable);
  RegisterSelectionEditor(THexViewCopyAction, TFWHexViewActionsSelectionEditor);
  RegisterSelectionEditor(THexViewByteViewModeAction, TFWHexViewActionsSelectionEditor);
  {$ENDIF}
  RegisterActions('HexView Actions', [THexViewCopyAction, THexViewByteViewModeAction], nil);
  RegisterComponents('FWControls', [TFWHexView, TMappedHexView]);
end;

{$IFDEF FPC}

initialization

//  {$I FWHexView.lrs}
  {$R fwhexview.res}


{$ELSE}

{ TFWHexViewActionsSelectionEditor }

procedure TFWHexViewActionsSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('FWHexView.Actions');
end;

{$ENDIF}

end.
