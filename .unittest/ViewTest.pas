unit ViewTest;

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  DUnitX.Assert,
  FWHexView.MappedView;

type
  TUnitTestView = class(TCustomMappedHexView);
  TUniTestRegion = class(TRegion)
  public
    property Level;
  end;
  TUniTestDataMap = class(TDataMap)
  public
    property Data;
  end;

  [TestFixture]
  TMyTestObject = class
  private
    FView: TUnitTestView;
    FData: TMemoryStream;
    function Region: TUniTestRegion;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestRegionRaiseTopIntersect;
    [Test]
    procedure TestRegionRaiseBottomIntersect;
    [Test]
    procedure TestThreeRegion;
    [Test]
    procedure TestRegionInPage;
    [Test]
    [TestCase('TestRegionSortAlignUpA', '1, 2, 3, 4')]
    [TestCase('TestRegionSortAlignUpB', '1, 2, 4, 3')]
    [TestCase('TestRegionSortAlignUpC', '1, 3, 2, 4')]
    [TestCase('TestRegionSortAlignUpD', '1, 3, 4, 2')]
    [TestCase('TestRegionSortAlignUpE', '1, 4, 2, 3')]
    [TestCase('TestRegionSortAlignUpF', '1, 4, 3, 2')]
    [TestCase('TestRegionSortAlignUpG', '2, 1, 3, 4')]
    [TestCase('TestRegionSortAlignUpH', '2, 1, 4, 3')]
    [TestCase('TestRegionSortAlignUpI', '2, 3, 1, 4')]
    [TestCase('TestRegionSortAlignUpJ', '2, 3, 4, 1')]
    [TestCase('TestRegionSortAlignUpK', '2, 4, 1, 3')]
    [TestCase('TestRegionSortAlignUpL', '2, 4, 3, 1')]
    [TestCase('TestRegionSortAlignUpM', '3, 1, 2, 4')]
    [TestCase('TestRegionSortAlignUpN', '3, 1, 4, 2')]
    [TestCase('TestRegionSortAlignUpO', '3, 2, 1, 4')]
    [TestCase('TestRegionSortAlignUpP', '3, 2, 4, 1')]
    [TestCase('TestRegionSortAlignUpQ', '3, 4, 1, 2')]
    [TestCase('TestRegionSortAlignUpR', '3, 4, 2, 1')]
    [TestCase('TestRegionSortAlignUpS', '4, 1, 2, 3')]
    [TestCase('TestRegionSortAlignUpT', '4, 1, 3, 2')]
    [TestCase('TestRegionSortAlignUpU', '4, 2, 1, 3')]
    [TestCase('TestRegionSortAlignUpV', '4, 2, 3, 1')]
    [TestCase('TestRegionSortAlignUpW', '4, 3, 1, 2')]
    [TestCase('TestRegionSortAlignUpX', '4, 3, 2, 1')]
    procedure TestRegionSortAlignUp(A, B, C, D: Integer);
    [Test]
    [TestCase('TestRegionSortAlignDownA', '1, 2, 3, 4')]
    [TestCase('TestRegionSortAlignDownB', '1, 2, 4, 3')]
    [TestCase('TestRegionSortAlignDownC', '1, 3, 2, 4')]
    [TestCase('TestRegionSortAlignDownD', '1, 3, 4, 2')]
    [TestCase('TestRegionSortAlignDownE', '1, 4, 2, 3')]
    [TestCase('TestRegionSortAlignDownF', '1, 4, 3, 2')]
    [TestCase('TestRegionSortAlignDownG', '2, 1, 3, 4')]
    [TestCase('TestRegionSortAlignDownH', '2, 1, 4, 3')]
    [TestCase('TestRegionSortAlignDownI', '2, 3, 1, 4')]
    [TestCase('TestRegionSortAlignDownJ', '2, 3, 4, 1')]
    [TestCase('TestRegionSortAlignDownK', '2, 4, 1, 3')]
    [TestCase('TestRegionSortAlignDownL', '2, 4, 3, 1')]
    [TestCase('TestRegionSortAlignDownM', '3, 1, 2, 4')]
    [TestCase('TestRegionSortAlignDownN', '3, 1, 4, 2')]
    [TestCase('TestRegionSortAlignDownO', '3, 2, 1, 4')]
    [TestCase('TestRegionSortAlignDownP', '3, 2, 4, 1')]
    [TestCase('TestRegionSortAlignDownQ', '3, 4, 1, 2')]
    [TestCase('TestRegionSortAlignDownR', '3, 4, 2, 1')]
    [TestCase('TestRegionSortAlignDownS', '4, 1, 2, 3')]
    [TestCase('TestRegionSortAlignDownT', '4, 1, 3, 2')]
    [TestCase('TestRegionSortAlignDownU', '4, 2, 1, 3')]
    [TestCase('TestRegionSortAlignDownV', '4, 2, 3, 1')]
    [TestCase('TestRegionSortAlignDownW', '4, 3, 1, 2')]
    [TestCase('TestRegionSortAlignDownX', '4, 3, 2, 1')]
    procedure TestRegionSortAlignDown(A, B, C, D: Integer);
    [Test]
    procedure TestRegionChange;
    [Test]
    procedure TestSeparatorsInRegion;
    [Test]
    procedure TestSeparatorsAnsMask;
  end;

implementation

function TMyTestObject.Region: TUniTestRegion;
begin
  Result := TUniTestRegion(FView.RawData.Region);
end;

procedure TMyTestObject.Setup;
begin
  FView := TUnitTestView.Create(nil);
  FData := TMemoryStream.Create;
  FData.SetSize(100);
  FView.SetDataStream(FData, 0, soOwned);
end;

procedure TMyTestObject.TearDown;
begin
  FView.Free;
end;

procedure TMyTestObject.TestRegionChange;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  M.SetSize(2000);
  FView.SetDataStream(M, 0, soOwned);
  with FView.DataMap.AddRegion($00, $400, '') do
  begin
    Header.Text := '0';
    FooterVisible := True;
    Footer.Text := '1';
  end;

  with FView.DataMap.AddRegion($00, $100, '') do
  begin
    Header.Text := '2';
    FooterVisible := True;
    Footer.Text := '3';
  end;

 Assert.AreEqual(FView.RawData[0].Description, '0');
 Assert.AreEqual(FView.RawData[1].Description, '2');
 Assert.AreEqual(FView.RawData[18].Description, '3');
 Assert.AreEqual(FView.RawData[67].Description, '1');
end;

procedure TMyTestObject.TestRegionInPage;
begin
  FView.BeginUpdate;
  FView.Pages.AddPage('Page 1', FView.BytesInRow, 50);
  FView.DataMap.AddRegion(FView.BytesInRow, 10, 'Region 1');
  FView.EndUpdate;

  Assert.IsTrue(FView.RowStyle(0) = rsRaw, '0 rsRaw');
  Assert.IsTrue(FView.RowStyle(1) = rsNone, '1 rsNone');
  Assert.IsTrue(FView.RowStyle(2) = rsSeparator, '2 rsSeparator');
  Assert.IsTrue(FView.RowStyle(3) = rsNone, '3 rsNone');
  Assert.IsTrue(FView.RowStyle(4) = rsRegion, '4 rsRegion');
end;

procedure TMyTestObject.TestRegionRaiseBottomIntersect;
var
  Region: TRegion;
begin
  Region := FView.DataMap.AddRegion(10, 20, 'Region 2');
  Assert.IsTrue(TUniTestDataMap(FView.DataMap).CompareRegion(0, 15, Region) =
    rcrPartialIntersectTop, 'Expected rcrPartialIntersectTop');
end;

procedure TMyTestObject.TestRegionRaiseTopIntersect;
var
  Region: TRegion;
begin
  Region := FView.DataMap.AddRegion(0, 20, 'Region 2');
  Assert.IsTrue(TUniTestDataMap(FView.DataMap).CompareRegion(10, 50, Region) =
    rcrPartialIntersectBottom, 'Expected rcrPartialIntersectBottom');
end;

procedure TMyTestObject.TestRegionSortAlignDown(A, B, C, D: Integer);

  procedure Add(Value: Integer);
  begin
    case Value of
      1: FView.DataMap.AddRegion($10, $50, '1', True).FooterVisible := True;
      2: FView.DataMap.AddRegion($40, $20, '2', True).FooterVisible := True;
      3: FView.DataMap.AddRegion($46, $1A, '3', True).FooterVisible := True;
      4: FView.DataMap.AddRegion($47, $19, '4', True).FooterVisible := True;
    end;
  end;

begin
  Add(A);
  Add(B);
  Add(C);
  Add(D);

  Assert.IsTrue(FView.RowStyle(0) = rsRaw, '0 rsRaw');
  Assert.IsTrue(FView.RawData[1].Style = rsRegion, '1 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '1');
  Assert.AreEqual(Region.Level, 1);
  Assert.IsTrue(FView.RawData[2].Style = rsRaw, '10 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[3].Style = rsRaw, '20 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[4].Style = rsRaw, '30 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[5].Style = rsRegion, '2 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '2');
  Assert.AreEqual(Region.Level, 2);
  Assert.IsTrue(FView.RawData[6].Style = rsRaw, '40 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 6);
  Assert.IsTrue(FView.RawData[7].Style = rsRegion, '3 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '3');
  Assert.AreEqual(Region.Level, 3);
  Assert.IsTrue(FView.RawData[8].Style = rsRaw, '46 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 1);
  Assert.IsTrue(FView.RawData[9].Style = rsRegion, '4 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '4');
  Assert.AreEqual(Region.Level, 4);
  Assert.IsTrue(FView.RawData[10].Style = rsRaw, '47 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.AreEqual(Integer(FView.RawData.Address), $47);
  Assert.IsTrue(FView.RawData[11].Style = rsRaw, '57 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 9);
  Assert.IsTrue(FView.RawData[12].Style = rsRegion, '4 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '4');
  Assert.AreEqual(Region.Level, 4);
  Assert.IsTrue(FView.RawData[13].Style = rsRegion, '3 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '3');
  Assert.AreEqual(Region.Level, 3);
  Assert.IsTrue(FView.RawData[14].Style = rsRegion, '2 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '2');
  Assert.AreEqual(Region.Level, 2);
  Assert.IsTrue(FView.RawData[15].Style = rsRegion, '1 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '1');
  Assert.AreEqual(Region.Level, 1);
  Assert.IsTrue(FView.RawData[16].Style = rsRaw, '60 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 4);
  Assert.AreEqual(Integer(FView.RawData.Address), $60);
end;

procedure TMyTestObject.TestRegionSortAlignUp(A, B, C, D: Integer);

  procedure Add(Value: Integer);
  begin
    case Value of
      1: FView.DataMap.AddRegion($10, $50, '1', True).FooterVisible := True;
      2: FView.DataMap.AddRegion($10, $20, '2', True).FooterVisible := True;
      3: FView.DataMap.AddRegion($10, $19, '3', True).FooterVisible := True;
      4: FView.DataMap.AddRegion($10, $18, '4', True).FooterVisible := True;
    end;
  end;

begin
  Add(A);
  Add(B);
  Add(C);
  Add(D);

  Assert.IsTrue(FView.RowStyle(0) = rsRaw, '0 rsRaw');
  Assert.IsTrue(FView.RawData[1].Style = rsRegion, '1 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '1');
  Assert.AreEqual(Region.Level, 1);
  Assert.IsTrue(FView.RawData[2].Style = rsRegion, '2 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '2');
  Assert.AreEqual(Region.Level, 2);
  Assert.IsTrue(FView.RawData[3].Style = rsRegion, '3 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '3');
  Assert.AreEqual(Region.Level, 3);
  Assert.IsTrue(FView.RawData[4].Style = rsRegion, '4 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '4');
  Assert.AreEqual(Region.Level, 4);
  Assert.IsTrue(FView.RawData[5].Style = rsRaw, '10 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[6].Style = rsRaw, '20 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 8);
  Assert.IsTrue(FView.RawData[7].Style = rsRegion, '4 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '4');
  Assert.AreEqual(Region.Level, 4);
  Assert.IsTrue(FView.RawData[8].Style = rsRaw, '28 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 1);
  Assert.AreEqual(Integer(FView.RawData.Address), $28);
  Assert.IsTrue(FView.RawData[9].Style = rsRegion, '3 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '3');
  Assert.AreEqual(Region.Level, 3);
  Assert.IsTrue(FView.RawData[10].Style = rsRaw, '29 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 7);
  Assert.IsTrue(FView.RawData[11].Style = rsRegion, '2 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '2');
  Assert.AreEqual(Region.Level, 2);
  Assert.IsTrue(FView.RawData[12].Style = rsRaw, '30 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[13].Style = rsRaw, '40 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.AreEqual(Integer(FView.RawData.Address), $40);
  Assert.IsTrue(FView.RawData[14].Style = rsRaw, '50 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 16);
  Assert.IsTrue(FView.RawData[15].Style = rsRegion, '1 rsRegion');
  Assert.AreEqual(FView.RawData.Description, '1');
  Assert.AreEqual(Region.Level, 1);
  Assert.IsTrue(FView.RawData[16].Style = rsRaw, '60 rsRaw');
  Assert.AreEqual(Integer(FView.RawData.RawLength), 4);
end;

procedure TMyTestObject.TestSeparatorsAnsMask;
begin
  FView.DataMap.AddSeparator($60, 'Separator');
  FView.DataMap.AddMask($5C, 4, 'Test mask', '', True);
  FView.DataMap.AddMaskCheck(0, '1', '1', True);
  Assert.IsTrue(FView.RawData[6].Style = rsMask, 'Mask start');
  Assert.IsTrue(FView.RawData[7].Style = rsMaskCheck, 'Mask check');
  Assert.IsTrue(FView.RawData[8].Style = rsSeparator, 'Separator');
end;

procedure TMyTestObject.TestSeparatorsInRegion;
begin
  FView.DataMap.AddSeparator($10, 'Out region header separator');
  FView.DataMap.AddSeparator($60, 'In region footer separator');
  FView.DataMap.AddRegion($10, $50, 'Region', True).FooterVisible := True;
  FView.DataMap.AddSeparator($60, 'Out region footer separator');
  FView.DataMap.AddSeparator($10, 'In region header separator');
  Assert.AreEqual(FView.RawData[1].Description, 'Out region header separator');
  Assert.IsTrue(FView.RawData[2].Style = rsRegion, 'Region start');
  Assert.AreEqual(FView.RawData[3].Description, 'In region header separator');
  Assert.AreEqual(FView.RawData[9].Description, 'In region footer separator');
  Assert.IsTrue(FView.RawData[10].Style = rsRegion, 'Region end');
  Assert.AreEqual(FView.RawData[11].Description, 'Out region footer separator');
end;

procedure TMyTestObject.TestThreeRegion;

  function GetRegionLevel(ARow: TMapRow): Integer;
  begin
    Result := TUniTestRegion(TUniTestDataMap(FView.DataMap).
      Regions[ARow.RegionIndex]).Level;
  end;

var
  I: Integer;
begin
  FView.BeginUpdate;
  FView.DataMap.AddRegion(0, 50, 'Region 1');
  FView.DataMap.AddRegion(0, 20, 'Region 2');
  FView.DataMap.AddRegion(0, 10, 'Region 3');
  FView.EndUpdate;
  Assert.AreEqual(TUniTestDataMap(FView.DataMap).Data[0].Description, 'Region 1');
  Assert.AreEqual(TUniTestDataMap(FView.DataMap).Data[1].Description, 'Region 2');
  Assert.AreEqual(TUniTestDataMap(FView.DataMap).Data[2].Description, 'Region 3');
  for I := 0 to 2 do
    Assert.AreEqual(I + 1, GetRegionLevel(TUniTestDataMap(FView.DataMap).Data[I]),
      '[' + TUniTestDataMap(FView.DataMap).Data[I].Description + ']');
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);

end.
