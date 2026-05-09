////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Reg.pas
//  * Purpose   : Registration module for FWHexView
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

unit FWHexView.Reg;

interface

{$IFDEF FPC}
  {$R fwhexview_fpc.res}
{$ELSE}
  {$R fwhexview.res}
{$ENDIF}

uses
  {$IFDEF FPC}
  ActnList,
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

{$ELSE}

{ TFWHexViewActionsSelectionEditor }

procedure TFWHexViewActionsSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('FWHexView.Actions');
end;

{$ENDIF}

end.
