unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, Menus, dDatenbank, SongsFormUnit, DB, Uni, Grids,
  FPImage, FPReadJPEG;  // <-- REQUIRED for JPG support

type

  { TAlbums }

  TAlbums = class(TForm)
    btnClearSearch: TSpeedButton;
    dbgAlbums: TDBGrid;
    dbMemoAlbumDescription: TDBMemo;
    edtAlbumSearch: TEdit;
    BackgroundImage: TImage;
    imgAlbumCover: TImage;
    dlgAlbumCover: TOpenDialog;
    miEditAlbum: TMenuItem;
    miViewTracks: TMenuItem;
    pmAlbum: TPopupMenu;

    procedure FormCreate(Sender: TObject);
    procedure btnAlbumSearchClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure edtAlbumSearchChange(Sender: TObject);
    procedure dbgAlbumsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dbgAlbumsCellClick(Column: TColumn);
    procedure miEditAlbumClick(Sender: TObject);
    procedure miViewTracksClick(Sender: TObject);
    procedure ImageLoadAlbumCover(Sender: TObject);

  private
    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
  end;

var
  Albums: TAlbums;

implementation

{$R *.lfm}

{ -------------------- FORM INIT -------------------- }

procedure TAlbums.FormCreate(Sender: TObject);
const
  BG_IMAGE_PATH =
    '/home/LAlbrecht/Entwicklung/Album/res/electric-guitar-with-neon-light.jpg';
begin
  dbgAlbums.PopupMenu := pmAlbum;

  { ---- BACKGROUND IMAGE ---- }
  BackgroundImage.Align := alClient;
  BackgroundImage.Stretch := True;
  BackgroundImage.Proportional := True;
  BackgroundImage.SendToBack;

  if FileExists(BG_IMAGE_PATH) then
    BackgroundImage.Picture.LoadFromFile(BG_IMAGE_PATH)
  else
    ShowMessage('Background image not found:'#10 + BG_IMAGE_PATH);

  { ---- AUTO database connection ---- }
  if Assigned(dmMain) then
  begin
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAlbum.Active then
      dmMain.qAlbum.Open;

    DisplayCurrentRecord;
  end;
end;



{ -------------------- DATASET UTIL -------------------- }

function TAlbums.CanEditDataset: Boolean;
begin
  Result :=
    Assigned(dmMain) and
    Assigned(dmMain.qAlbum) and
    dmMain.qAlbum.Active and
    not dmMain.qAlbum.IsEmpty;
end;

procedure TAlbums.DisplayCurrentRecord;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  imgAlbumCover.Picture := nil;

  Field := dmMain.qAlbum.FieldByName('ALBUMCOVER');
  if not Assigned(Field) or Field.IsNull then Exit;

  BlobStream := dmMain.qAlbum.CreateBlobStream(Field, bmRead);
  try
    BlobStream.Position := 0;
    imgAlbumCover.Picture.LoadFromStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

{ -------------------- SEARCH -------------------- }

procedure TAlbums.btnAlbumSearchClick(Sender: TObject);
begin
  if not CanEditDataset then Exit;

  dmMain.qAlbum.Filtered := False;
  dmMain.qAlbum.Filter :=
      '(Album LIKE ' + QuotedStr('%' + edtAlbumSearch.Text + '%') + ') OR ' +
      '(Artist LIKE ' + QuotedStr('%' + edtAlbumSearch.Text + '%') + ') OR ' +
      '(ReleaseYear LIKE ' + QuotedStr('%' + edtAlbumSearch.Text + '%') + ')';
  dmMain.qAlbum.Filtered := True;

  DisplayCurrentRecord;
end;

procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';
  if Assigned(dmMain.qAlbum) then
    dmMain.qAlbum.Filtered := False;
end;

procedure TAlbums.edtAlbumSearchChange(Sender: TObject);
begin
  btnAlbumSearchClick(Sender);
end;

{ -------------------- GRID -------------------- }

procedure TAlbums.dbgAlbumsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Coord: TGridCoord;
begin
  if Button = mbRight then
  begin
    Coord := dbgAlbums.MouseCoord(X, Y);
    if (Coord.Y > 0) and CanEditDataset then
      dmMain.qAlbum.RecNo := Coord.Y;
  end;
end;

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

{ -------------------- IMAGE WRITE -------------------- }

procedure TAlbums.ImageLoadAlbumCover(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
  Field: TField;
begin
  if not CanEditDataset then Exit;
  if not dlgAlbumCover.Execute then Exit;

  Field := dmMain.qAlbum.FieldByName('ALBUMCOVER');
  if not Assigned(Field) then
    raise Exception.Create('ALBUMCOVER field missing');

  dmMain.qAlbum.Edit;

  FileStream := TFileStream.Create(dlgAlbumCover.FileName, fmOpenRead);
  try
    BlobStream := dmMain.qAlbum.CreateBlobStream(Field, bmWrite);
    try
      BlobStream.CopyFrom(FileStream, FileStream.Size);
    finally
      BlobStream.Free;
    end;
  finally
    FileStream.Free;
  end;

  dmMain.qAlbum.Post;
  DisplayCurrentRecord;
end;

end.

