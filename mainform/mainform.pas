unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, Buttons, Menus, DB,
  dDatenbank, SongsFormUnit, AlbumModel, DBCtrls, Grids;

type

  { TAlbums }

  TAlbums = class(TForm)
    btnClearSearch:         TSpeedButton;
    dbgAlbums:              TDBGrid;
    dbMemoAlbumDescription: TDBMemo;
    edtAlbumSearch:         TEdit;
    imgAlbumCover:          TImage;
    pmAlbum:                TPopupMenu;
    miEditAlbum:            TMenuItem;
    miViewTracks:           TMenuItem;
    dlgAlbumCover:          TOpenDialog;
    BackgroundImage:        TImage;

    procedure FormCreate          (Sender: TObject);
    procedure edtAlbumSearchChange(Sender: TObject);
    procedure btnClearSearchClick (Sender: TObject);
    procedure dbgAlbumsCellClick  (Column: TColumn);
    procedure miEditAlbumClick    (Sender: TObject);
    procedure miViewTracksClick   (Sender: TObject);
    procedure ImageLoadAlbumCover (Sender: TObject);

  private
    FAlbumModel: TAlbumModel;

    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
    procedure SyncDatasetWithGrid;

    procedure AlbumAfterScroll(DataSet: TDataSet);
    procedure AlbumDataChange(Sender: TObject; Field: TField);
  end;

var
  Albums: TAlbums;

implementation

{$R *.lfm}

{ -------------------- INIT -------------------- }

procedure TAlbums.FormCreate(Sender: TObject);
begin
  if Assigned(dmMain) and Assigned(dmMain.qAlbum) then
  begin
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAlbum.Active then
      dmMain.qAlbum.Open;

    FAlbumModel := TAlbumModel.Create(dmMain.qAlbum);

    dmMain.qAlbum.AfterScroll := @AlbumAfterScroll;
    dmMain.qAlbum.DataSource.OnDataChange := @AlbumDataChange;

    // Only display if both dataset and model are valid
    if CanEditDataset then
      DisplayCurrentRecord;
  end;
end;

procedure TAlbums.AlbumDataChange(Sender: TObject; Field: TField);
begin
  DisplayCurrentRecord;
end;

{ -------------------- DATA -------------------- }

function TAlbums.CanEditDataset: Boolean;
begin
  Result :=
    Assigned(dmMain) and
    Assigned(dmMain.qAlbum) and
    dmMain.qAlbum.Active and
    not dmMain.qAlbum.IsEmpty;
end;

procedure TAlbums.SyncDatasetWithGrid;
var
  Coord: TGridCoord;
begin
  if not Assigned(dbgAlbums.DataSource) then Exit;

  Coord := dbgAlbums.MouseCoord(dbgAlbums.ScreenToClient(Mouse.CursorPos).X, dbgAlbums.ScreenToClient(Mouse.CursorPos).Y);
  if (Coord.Y > 0) and CanEditDataset then
    dmMain.qAlbum.RecNo := Coord.Y;
end;

procedure TAlbums.DisplayCurrentRecord;
var
  Stream: TStream;
begin
  if not Assigned(FAlbumModel) or not CanEditDataset then Exit;

  imgAlbumCover.Picture := nil;

  if not FAlbumModel.HasCover then Exit;

  Stream := FAlbumModel.CreateCoverStream;
  try
    imgAlbumCover.Picture.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAlbums.AlbumAfterScroll(DataSet: TDataSet);
begin
  DisplayCurrentRecord;
end;

{ -------------------- SEARCH -------------------- }

procedure TAlbums.edtAlbumSearchChange(Sender: TObject);
begin
  //btnClearSearch.Visible := edtAlbumSearch.Text <> '';

  if CanEditDataset then
  begin
    dmMain.qAlbum.Filtered := False;

    if edtAlbumSearch.Text <> '' then
    begin
      dmMain.qAlbum.Filter :=
        '(Album LIKE '  + QuotedStr('%' + edtAlbumSearch.Text + '%') + ') OR ' +
        '(Artist LIKE ' + QuotedStr('%' + edtAlbumSearch.Text + '%') + ')';
      dmMain.qAlbum.Filtered := True;
    end;

    DisplayCurrentRecord;
  end;
end;

procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';
  dmMain.qAlbum.Filtered := False;
  //btnClearSearch.Visible := False;
end;

{ -------------------- GRID -------------------- }

procedure TAlbums.dbgAlbumsCellClick(Column: TColumn);
begin
  DisplayCurrentRecord;
end;

{ -------------------- POPUP -------------------- }

procedure TAlbums.miEditAlbumClick(Sender: TObject);
begin
  if CanEditDataset then
    dmMain.qAlbum.Edit;
end;

procedure TAlbums.miViewTracksClick(Sender: TObject);
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

procedure TAlbums.ImageLoadAlbumCover(Sender: TObject);
begin
  if CanEditDataset then
  begin
    // Ensure the dataset points to the selected row
    //SyncDatasetWithGrid;
    FAlbumModel.LoadCoverFromDialog(imgAlbumCover, dlgAlbumCover);
  end;
end;

end.

