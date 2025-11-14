unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, Menus, dDatenbank, SongsFormUnit, DB, Uni, Grids,
  uConstants;

type
  { TAlbums: Main form for managing albums and their details }

  TAlbums = class(TForm)
    btnDBConnect:           TButton;
    btnLoadAlbumCover:      TButton;
    btnAlbumSearch:         TButton;
    btnClearSearch:         TSpeedButton;
    dbgAlbums:              TDBGrid;
    dbMemoAlbumDescription: TDBMemo;
    edtAlbumSearch:         TEdit;
    imgAlbumCover:          TImage;
    dlgAlbumCover:          TOpenDialog;
    miEditAlbum:            TMenuItem;
    miViewTracks:           TMenuItem;
    navAlbums:              TDBNavigator;
    pmAlbum:                TPopupMenu;

    procedure FormCreate(Sender: TObject);
    procedure btnDBConnectClick(Sender: TObject);
    procedure btnLoadAlbumCoverClick(Sender: TObject);
    procedure btnAlbumSearchClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure edtAlbumSearchChange(Sender: TObject);
    procedure dbgAlbumsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dbgAlbumsCellClick(Column: TColumn);
    procedure miEditAlbumClick(Sender: TObject);
    procedure miViewTracksClick(Sender: TObject);

  private
    { Displays the current selected record in the form controls }
    procedure DisplayCurrentRecord(DataSet: TDataSet = nil);

    { Checks if dataset is available and editable }
    function  CanEditDataset: Boolean;
  end;

var
  Albums: TAlbums;

implementation

{$R *.lfm}

uses
  Variants;

{ Form Initialization }

procedure TAlbums.FormCreate(Sender: TObject);
begin
  // Attach popup menu to DBGrid
  dbgAlbums.PopupMenu := pmAlbum;
end;

{ Dataset Utilities }

function TAlbums.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
         and Assigned(dmMain.qAdressen)
         and dmMain.qAdressen.Active
         and not dmMain.qAdressen.IsEmpty;
end;

procedure TAlbums.DisplayCurrentRecord(DataSet: TDataSet);
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  // Show album description
  dbMemoAlbumDescription.Text :=
    dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION).AsString;

  // Load album cover from BLOB field
  imgAlbumCover.Picture := nil;
  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  if Assigned(Field) and not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

{ Database Actions }

procedure TAlbums.btnDBConnectClick(Sender: TObject);
begin
  // Connect to database and open main album dataset
  if not Assigned(dmMain) then
  begin
    ShowMessage('Data module (dmMain) not assigned.');
    Exit;
  end;

  try
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAdressen.Active then
      dmMain.qAdressen.Open;

    DisplayCurrentRecord;
  except
    on E: Exception do
      ShowMessage('Database error: ' + E.Message);
  end;
end;

procedure TAlbums.btnAlbumSearchClick(Sender: TObject);
var
  FilterText: string;
begin
  // Filters albums based on search text in album or artist fields
  if not Assigned(dmMain) or not Assigned(dmMain.qAdressen) then Exit;

  FilterText := Trim(edtAlbumSearch.Text);

  if FilterText = '' then
  begin
    dmMain.qAdressen.Filtered := False;
    Exit;
  end;

  dmMain.qAdressen.Filtered := False;
  dmMain.qAdressen.Filter :=
    Format('(ALBUM LIKE ''%%%s%%'') OR (ARTIST LIKE ''%%%s%%'')',
           [FilterText, FilterText]);
  dmMain.qAdressen.Filtered := True;

  if not dmMain.qAdressen.IsEmpty then
    DisplayCurrentRecord;
end;

procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  // Clears search input and removes filtering
  edtAlbumSearch.Text := '';
  edtAlbumSearch.SetFocus;
  if Assigned(dmMain) and Assigned(dmMain.qAdressen) then
    dmMain.qAdressen.Filtered := False;
  btnClearSearch.Visible := False;
end;

procedure TAlbums.edtAlbumSearchChange(Sender: TObject);
begin
  // Triggered on search text change: updates filtering and clear button visibility
  if Assigned(btnClearSearch) then
    btnClearSearch.Visible := edtAlbumSearch.Text <> '';
  btnAlbumSearchClick(Sender);
end;

{ Grid Right-Click Handling }

procedure TAlbums.dbgAlbumsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Coord: TGridCoord;
begin
  // Select the row under right-click in Lazarus DBGrid
  if Button = mbRight then
  begin
    Coord := dbgAlbums.MouseCoord(X, Y);
    if (Coord.Y > 0) and CanEditDataset then
      dmMain.qAdressen.RecNo := Coord.Y; // select row under mouse
  end;
end;

{ Grid Left-Click Handling }

procedure TAlbums.dbgAlbumsCellClick(Column: TColumn);
begin
  // Display current album details when left-clicking a row
  if CanEditDataset then
    DisplayCurrentRecord;
end;

{ Popup Menu Actions }

procedure TAlbums.miEditAlbumClick(Sender: TObject);
begin
  // Edit album from right-click menu
  if CanEditDataset then
    dmMain.qAdressen.Edit;
end;

procedure TAlbums.miViewTracksClick(Sender: TObject);
begin
  // View tracks for album from right-click menu
  if CanEditDataset then
  begin
    if not Assigned(Tracks) then
      Application.CreateForm(TTracks, Tracks);

    Tracks.LoadSongsFromAlbum(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
    Tracks.Show;
  end;
end;

{ UI Actions }

procedure TAlbums.btnLoadAlbumCoverClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  // Loads a new album cover image into the database
  if not CanEditDataset then Exit;
  if not dlgAlbumCover.Execute then Exit;

  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  if not Assigned(Field) or (Field.DataType <> ftBlob) then
    raise Exception.Create('AlbumCover field missing or not a BLOB');

  dmMain.qAdressen.Edit;
  FileStream := TFileStream.Create(dlgAlbumCover.FileName, fmOpenRead);
  try
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmWrite);
    try
      BlobStream.CopyFrom(FileStream, FileStream.Size);
    finally
      BlobStream.Free;
    end;
  finally
    FileStream.Free;
  end;

  dmMain.qAdressen.Post;
  DisplayCurrentRecord;
  ShowMessage('Image saved successfully!');
end;
//TODO: DB connection should happen after succesful login and not with clicking connect button
//DONE: right click to choose from a menue: Viewtracks &/ Edit Album

end.



