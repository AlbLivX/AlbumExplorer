unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, Buttons, Menus, DB,
  dDatenbank, SongsFormUnit, AlbumModel, DBCtrls, Grids, ExtDlgs;

type

  { TpmAlbum }

  TpmAlbum = class(TForm)
    dbgAlbums:              TDBGrid;
    edtAlbumSearch:         TEdit;
    imgAlbumCover:          TImage;
    dlgAlbumCover:          TOpenPictureDialog;
    pmAlbum:                TPopupMenu;
    miEditAlbum:            TMenuItem;
    miViewTracks:           TMenuItem;
    btnClearSearch:         TSpeedButton;
    dbMemoAlbumDescription: TDBMemo;

    procedure FormCreate           (Sender: TObject);
    procedure edtAlbumSearchChange (Sender: TObject);
    procedure btnClearSearchClick  (Sender: TObject);
    procedure dbgAlbumsCellClick   (Column: TColumn);
    procedure miEditAlbumClick     (Sender: TObject);
    procedure miViewTracksClick    (Sender: TObject);
    procedure ImageLoadAlbumCover  (Sender: TObject);
    procedure btnClearSearchVisible(Sender: TObject);
    procedure edtAlbumSearchExit   (Sender: TObject);


  private
    FAlbumModel: TAlbumModel;

    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
    procedure SyncDatasetWithGrid;

    procedure AlbumAfterScroll(DataSet: TDataSet);
    procedure AlbumDataChange (Sender: TObject; Field: TField);
  end;

var
  pmAlbum: TpmAlbum;

implementation

{$R *.lfm}

{ -------------------- INIT -------------------- }

procedure TpmAlbum.FormCreate(Sender: TObject);
begin
  if Assigned(dmMain) and Assigned(dmMain.qAlbum) then
  begin
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAlbum.Active then
      dmMain.qAlbum.Open;

    FAlbumModel := TAlbumModel.Create(dmMain.qAlbum);

    // Hook dataset events for UI updates
    dmMain.qAlbum.AfterScroll := @AlbumAfterScroll;
    if Assigned(dmMain.qAlbum.DataSource) then
      dmMain.qAlbum.DataSource.OnDataChange := @AlbumDataChange;

    // Only display if both dataset and model are valid
    if CanEditDataset then
      DisplayCurrentRecord;
  end;
end;

procedure TpmAlbum.AlbumDataChange(Sender: TObject; Field: TField);
begin
  DisplayCurrentRecord;
end;

{ -------------------- DATA -------------------- }

function TpmAlbum.CanEditDataset: Boolean;
begin
  Result :=
    Assigned(dmMain) and
    Assigned(dmMain.qAlbum) and
    dmMain.qAlbum.Active and
    not dmMain.qAlbum.IsEmpty;
end;

procedure TpmAlbum.SyncDatasetWithGrid;
var
  Coord: TGridCoord;
begin
  if not Assigned(dbgAlbums.DataSource) then Exit;

  Coord := dbgAlbums.MouseCoord(dbgAlbums.ScreenToClient(Mouse.CursorPos).X, dbgAlbums.ScreenToClient(Mouse.CursorPos).Y);
  if (Coord.Y > 0) and CanEditDataset then
    dmMain.qAlbum.RecNo := Coord.Y;
end;

procedure TpmAlbum.DisplayCurrentRecord;
var
  Stream: TStream;
begin
  if not Assigned(FAlbumModel) or not CanEditDataset then Exit;

  imgAlbumCover.Picture := nil;

  if not FAlbumModel.HasCover then Exit;

  Stream := FAlbumModel.CreateCoverStream;
  try
    imgAlbumCover.Picture.LoadFromStream(Stream);
    imgAlbumCover.Invalidate;
    dbMemoAlbumDescription.Invalidate;
  finally
    Stream.Free;
  end;
end;

procedure TpmAlbum.AlbumAfterScroll(DataSet: TDataSet);
begin
  DisplayCurrentRecord;
end;

{ -------------------- SEARCH -------------------- }


procedure TpmAlbum.edtAlbumSearchChange(Sender: TObject);
begin
  if CanEditDataset then
  begin
    // Update the query parameter and reopen dataset
    dmMain.qAlbum.Close;
    dmMain.qAlbum.ParamByName('SEARCH').AsString := '%' + edtAlbumSearch.Text + '%';
    dmMain.qAlbum.Open;

    // Ensure cursor points to first record
    if CanEditDataset then
      dmMain.qAlbum.First;

    DisplayCurrentRecord;
  end;
end;

procedure TpmAlbum.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';

  dmMain.qAlbum.Close;
  dmMain.qAlbum.ParamByName('SEARCH').AsString := '%%';
  dmMain.qAlbum.Open;

  if CanEditDataset then
    dmMain.qAlbum.First;

  DisplayCurrentRecord;
end;

procedure TpmAlbum.btnClearSearchVisible(Sender: TObject);
begin
  btnClearSearch.Visible := True;
end;

procedure TpmAlbum.edtAlbumSearchExit(Sender: TObject);
begin
  btnClearSearch.Visible := False;
end;


{ -------------------- GRID -------------------- }

procedure TpmAlbum.dbgAlbumsCellClick(Column: TColumn);
begin
  DisplayCurrentRecord;
end;



{ -------------------- POPUP -------------------- }

procedure TpmAlbum.miEditAlbumClick(Sender: TObject);
begin
  if CanEditDataset then
    dmMain.qAlbum.Edit;
end;

procedure TpmAlbum.miViewTracksClick(Sender: TObject);
var
  AlbumID: Integer;
begin
  if not CanEditDataset then Exit;

  AlbumID := dmMain.qAlbum.FieldByName('ID').AsInteger;

  if not Assigned(Tracks) then
    Tracks := TTracks.Create(Application);

  Tracks.LoadSongsFromAlbum(AlbumID);
  Tracks.Show;
end;

{ -------------------- ALBUM COVER -------------------- }

procedure TpmAlbum.ImageLoadAlbumCover(Sender: TObject);
begin
  if CanEditDataset then
  begin
    // Ensure the dataset points to the selected row
    SyncDatasetWithGrid;
    FAlbumModel.LoadCoverFromDialog(imgAlbumCover, dlgAlbumCover);
  end;
end;

end.

