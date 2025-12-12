unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, Menus, dDatenbank, SongsFormUnit, DB, Uni, Grids;

type
  { TAlbums: Main form for managing albums and their details }
  TAlbums = class(TForm)
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

    procedure btnClearSearchClickClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TAlbums.btnClearSearchClickClick(Sender: TObject);
begin

end;

{ Dataset Utilities }
function TAlbums.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
         and Assigned(dmMain.qAlbum)
         and dmMain.qAlbum.Active
         and not dmMain.qAlbum.IsEmpty;
end;

procedure TAlbums.DisplayCurrentRecord(DataSet: TDataSet);
var
  Field: TBlobField;
  BlobStream: TMemoryStream;
begin
  if not CanEditDataset then Exit;

  // Show album description
  dbMemoAlbumDescription.Text := dmMain.qAlbum.FieldByName('DESCRIPTION').AsString;

  // Load album cover
  Field := dmMain.qAlbum.FieldByName('ALBUMCOVER') as TBlobField;
  imgAlbumCover.Picture := nil;
  if Assigned(Field) and not Field.IsNull then
  begin
    BlobStream := TMemoryStream.Create;
    try
      Field.SaveToStream(BlobStream);   // safer than CreateBlobStream
      BlobStream.Position := 0;
      imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;


{ Database Actions }


procedure TAlbums.btnAlbumSearchClick(Sender: TObject);
var
  FilterText: string;
begin
  // Filters albums based on search text in album or artist fields
  if not Assigned(dmMain) or not Assigned(dmMain.qAlbum) then Exit;

  FilterText := Trim(edtAlbumSearch.Text);

  if FilterText = '' then
  begin
    dmMain.qAlbum.Filtered := False;
    Exit;
  end;

  dmMain.qAlbum.Filtered := False;
  dmMain.qAlbum.Filtered := True;

  if not dmMain.qAlbum.IsEmpty then
    DisplayCurrentRecord;
end;

procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  // Clears search input and removes filtering
  edtAlbumSearch.Text := '';
  edtAlbumSearch.SetFocus;
  if Assigned(dmMain) and Assigned(dmMain.qAlbum) then
    dmMain.qAlbum.Filtered := False;
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
      dmMain.qAlbum.RecNo := Coord.Y; // select row under mouse
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
  if CanEditDataset then
  begin
    // Put dataset into edit mode
    dmMain.qAlbum.Edit;

    // Skip ID column: start editing the first editable column (index 1 or by field name)
    dbgAlbums.SelectedIndex := 1;

    // Enable editing in the grid
    dbgAlbums.Options := dbgAlbums.Options + [dgEditing];
    dbgAlbums.EditorMode := True;
  end;
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

{ UI Actions }
procedure TAlbums.btnLoadAlbumCoverClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobField: TBlobField;
begin
  if not CanEditDataset then Exit;
  if not dlgAlbumCover.Execute then Exit;

  BlobField := dmMain.qAlbum.FieldByName('ALBUMCOVER') as TBlobField;
  if not Assigned(BlobField) then
    raise Exception.Create('ALBUMCOVER field missing');

  dmMain.qAlbum.Edit;

  FileStream := TFileStream.Create(dlgAlbumCover.FileName, fmOpenRead);
  try
    BlobField.LoadFromStream(FileStream);   // Load image directly into field
  finally
    FileStream.Free;
  end;

  dmMain.qAlbum.Post;
  DisplayCurrentRecord;
  ShowMessage('Image saved successfully!');
end;

end.
