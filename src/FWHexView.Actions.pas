////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Actions.pas
//  * Purpose   : Basic actions for FWHexView
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

unit FWHexView.Actions;

{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  ActnList,
  FWHexView.Common;

type
  THexViewActionExecuteTargetEvent = procedure(Sender, Target: TObject) of object;
  THexViewActionHandlesTargetEvent = procedure(Sender, Target: TObject; var AHandled: Boolean) of object;
  THexViewActionUpdateTargetEvent = procedure(Sender, Target: TObject; var AEnabled, AChecked: Boolean) of object;

const
  IID_HexViewCopyAction = '{C81EA234-725C-4AF5-9A37-7E601BF751BF}';
  IID_HexViewByteViewModeAction = '{A03E6086-45F6-4A1B-BCDE-B670CB4362AC}';

type
  IHexViewCopyAction = interface
    [IID_HexViewCopyAction]
    function CopyCommandEnabled(Value: TCopyStyle): Boolean;
    function CopyCommandHandled(Value: TCopyStyle): Boolean;
    procedure CopySelected(Value: TCopyStyle);
  end;

  IHexViewByteViewModeAction = interface
    [IID_HexViewByteViewModeAction]
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean;
    function ByteViewModeCommandHandled(Value: TByteViewMode): Boolean;
    procedure SetByteViewMode(Value: TByteViewMode);
  end;

  THexViewBasicAction = class(TCustomAction)
  private
    FOnExecuteTarget: THexViewActionExecuteTargetEvent;
    FOnHandlesTarget: THexViewActionHandlesTargetEvent;
    FOnUpdateTarget: THexViewActionUpdateTargetEvent;
  protected
    procedure DoHandlesTarget(Target: TObject; var AHandled: Boolean); virtual;
    procedure DoUpdateTarget(Target: TObject; var AEnabled, AChecked: Boolean); virtual;
    procedure DoExecuteTarget(Target: TObject); virtual;
    property OnExecuteTarget: THexViewActionExecuteTargetEvent read FOnExecuteTarget write FOnExecuteTarget;
    property OnHandlesTarget: THexViewActionHandlesTargetEvent read FOnHandlesTarget write FOnHandlesTarget;
    property OnUpdateTarget: THexViewActionUpdateTargetEvent read FOnUpdateTarget write FOnUpdateTarget;
  end;

  THexViewCustomCopyAction = class(THexViewBasicAction)
  private const
    stCaptions: array [TCopyStyle] of string = (
      'Copy as Text', 'Copy Address', 'Copy Bytes', 'Copy Pas', 'Copy C++', 'Copy Asm');
    stHints: array [TCopyStyle] of string = (
      'Copy selected data as text', 'Copy Address', 'Copy selected data as bytes',
      'Copy selected data as Pascal array', 'Copy selected data as C array',
      'Copy selected data as Assembler opcodes (DB 0x...)');
    scShortcuts: array [TCopyStyle] of TShortCut = (Byte('C') or scCtrl, 0, 0, 0, 0, 0);
  private
    FCopyStyle: TCopyStyle;
    procedure SetCopyStyle(const Value: TCopyStyle);
    procedure UpdateCopyStyle(const Value: TCopyStyle);
  protected
    function IsCaptionStored: Boolean; virtual;
    function IsHintStored: Boolean; virtual;
    function IsShortCutStored: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  protected
    property Caption stored IsCaptionStored;
    property CopyStyle: TCopyStyle read FCopyStyle write SetCopyStyle default csAsText;
    property Hint stored IsHintStored;
    property ShortCut stored IsShortCutStored;
  end;

  THexViewCopyAction = class(THexViewCustomCopyAction)
  published
    property Caption;
    property CopyStyle;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecuteTarget;
    property OnHandlesTarget;
    property OnHint;
    property OnUpdateTarget;
  end;

  THexViewCustomByteViewModeAction = class(THexViewBasicAction)
  private const
    stCaptions: array [TByteViewMode] of string = (
      'Hex Byte (8-bit)', 'Hex Short (16-bit)', 'Hex Long (32-bit)',
      'Hex Long Long (64-bit)', 'Signed Byte (8-bit)',
      'Signed Short (16-bit)', 'Signed Long (32-bit)',
      'Signed Long Long (64-bit)', 'Unsigned Byte (8-bit)',
      'Unsigned Short (16-bit)', 'Unsigned Long (32-bit)',
      'Unsigned Long Long (64-bit)', 'Float (32-bit)', 'Double (64-bit)',
      'Long Double (80-bit)', 'Text', 'Address');
  private
    FByteViewMode: TByteViewMode;
    procedure UpdateByteViewMode(const Value: TByteViewMode);
    procedure SetByteViewMode(const Value: TByteViewMode);
  protected
    function IsCaptionStored: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  protected
    property Caption stored IsCaptionStored;
    property ByteViewMode: TByteViewMode read FByteViewMode write SetByteViewMode default bvmHex8;
  end;

  THexViewByteViewModeAction = class(THexViewCustomByteViewModeAction)
  published
    property ByteViewMode;
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecuteTarget;
    property OnHandlesTarget;
    property OnHint;
    property OnUpdateTarget;
  end;

implementation

{ THexViewBasicAction }

procedure THexViewBasicAction.DoExecuteTarget(Target: TObject);
begin
  if Assigned(FOnExecuteTarget) then
    FOnExecuteTarget(Self, Target);
end;

procedure THexViewBasicAction.DoHandlesTarget(Target: TObject;
  var AHandled: Boolean);
begin
  if Assigned(FOnHandlesTarget) then
    FOnHandlesTarget(Self, Target, AHandled);
end;

procedure THexViewBasicAction.DoUpdateTarget(Target: TObject;
  var AEnabled, AChecked: Boolean);
begin
  if Assigned(FOnUpdateTarget) then
    FOnUpdateTarget(Self, Target, AEnabled, AChecked);
end;

{ THexViewCustomCopyAction }

constructor THexViewCustomCopyAction.Create(AOwner: TComponent);
begin
  inherited;
  UpdateCopyStyle(CopyStyle);
end;

procedure THexViewCustomCopyAction.ExecuteTarget(Target: TObject);
var
  Intf: IHexViewCopyAction;
begin
  if csDesigning in ComponentState then Exit;
  if Assigned(Target) and Supports(Target, IHexViewCopyAction, Intf) then
    Intf.CopySelected(CopyStyle);
  DoExecuteTarget(Target);
end;

function THexViewCustomCopyAction.HandlesTarget(Target: TObject): Boolean;
var
  Intf: IHexViewCopyAction;
begin
  if csDesigning in ComponentState then Exit(True);
  Result :=
    Assigned(Target) and
    Supports(Target, IHexViewCopyAction, Intf) and
    Intf.CopyCommandHandled(CopyStyle);
  DoHandlesTarget(Target, Result);
end;

function THexViewCustomCopyAction.IsCaptionStored: Boolean;
begin
  Result := Caption <> stCaptions[CopyStyle];
end;

function THexViewCustomCopyAction.IsHintStored: Boolean;
begin
  Result := Hint <> stHints[CopyStyle];
end;

function THexViewCustomCopyAction.IsShortCutStored: Boolean;
begin
  Result := ShortCut <> scShortcuts[CopyStyle];
end;

procedure THexViewCustomCopyAction.SetCopyStyle(const Value: TCopyStyle);
begin
  if CopyStyle <> Value then
  begin
    FCopyStyle := Value;
    UpdateCopyStyle(Value);
  end;
end;

procedure THexViewCustomCopyAction.UpdateCopyStyle(const Value: TCopyStyle);
begin
  Caption := stCaptions[Value];
  Hint := stHints[Value];
  ShortCut := scShortcuts[Value];
end;

procedure THexViewCustomCopyAction.UpdateTarget(Target: TObject);
var
  Intf: IHexViewCopyAction;
  AEnabled, AChecked: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    Enabled := True;
    Exit;
  end;
  AEnabled :=
    Assigned(Target) and
    Supports(Target, IHexViewCopyAction, Intf) and
    Intf.CopyCommandEnabled(CopyStyle);
  AChecked := False;
  DoUpdateTarget(Target, AEnabled, AChecked);
  Enabled := AEnabled;
end;

{ THexViewCustomByteViewModeAction }

constructor THexViewCustomByteViewModeAction.Create(AOwner: TComponent);
begin
  inherited;
  UpdateByteViewMode(ByteViewMode);
end;

procedure THexViewCustomByteViewModeAction.ExecuteTarget(Target: TObject);
var
  Intf: IHexViewByteViewModeAction;
begin
  if csDesigning in ComponentState then Exit;
  if Assigned(Target) and Supports(Target, IHexViewByteViewModeAction, Intf) then
    Intf.SetByteViewMode(ByteViewMode);
  DoExecuteTarget(Target);
end;

function THexViewCustomByteViewModeAction.HandlesTarget(
  Target: TObject): Boolean;
var
  Intf: IHexViewByteViewModeAction;
begin
  if csDesigning in ComponentState then Exit(True);
  Result :=
    Assigned(Target) and
    Supports(Target, IHexViewByteViewModeAction, Intf) and
    Intf.ByteViewModeCommandHandled(ByteViewMode);
  DoHandlesTarget(Target, Result);
end;

function THexViewCustomByteViewModeAction.IsCaptionStored: Boolean;
begin
  Result := Caption <> stCaptions[ByteViewMode];
end;

procedure THexViewCustomByteViewModeAction.SetByteViewMode(
  const Value: TByteViewMode);
begin
  if ByteViewMode <> Value then
  begin
    FByteViewMode := Value;
    UpdateByteViewMode(ByteViewMode);
  end;
end;

procedure THexViewCustomByteViewModeAction.UpdateByteViewMode(
  const Value: TByteViewMode);
begin
  Caption := stCaptions[Value];
end;

procedure THexViewCustomByteViewModeAction.UpdateTarget(Target: TObject);
var
  Intf: IHexViewByteViewModeAction;
  AEnabled, AChecked: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    Enabled := True;
    Exit;
  end;
  AChecked := False;
  AEnabled :=
    Assigned(Target) and
    Supports(Target, IHexViewByteViewModeAction, Intf) and
    Intf.ByteViewModeCommandEnabled(ByteViewMode, AChecked);
  DoUpdateTarget(Target, AEnabled, AChecked);
  Enabled := AEnabled;
  Checked := AChecked;
end;

end.
