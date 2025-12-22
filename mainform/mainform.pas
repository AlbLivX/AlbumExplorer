unit MainFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, Buttons, Menus, DB,
  dDatenbank, SongsFormUnit, DBCtrls, Grids, ExtDlgs, Types,
  AlbumModel;

type
  { TpmAlbum }

  TpmAlbum = class(TForm)
    dbgAlbums:              TDBGrid;
    edtAlbumSearch:         TEdit;
    dlgAlbumCover:          TOpenPictureDialog;
    imgAlbumCover:          TImage;
    lblDescription:         TLabel;
    lblSearch:              TLabel;
    pmAlbum:                TPopupMenu;
    miEditAlbum:            TMenuItem;
    miViewTracks:           TMenuItem;
    btnClearSearch:         TSpeedButton;
    dbMemoAlbumDescription: TDBMemo;

    procedure FormCreate(Sender: TObject);
    procedure edtAlbumSearchChange(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnClearSearchVisible(Sender: TObject);
    procedure edtAlbumSearchExit(Sender: TObject);
    procedure dbgAlbumsCellClick(Column: TColumn);
    procedure imgAlbumCoverClick(Sender: TObject);
    procedure miEditAlbumClick(Sender: TObject);
    procedure miViewTracksClick(Sender: TObject);

  private
    FAlbumModel: TAlbumModel;
    procedure AlbumDataChange(Sender: TObject; Field: TField);
    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
  public

  end;

var
  pmAlbum: TpmAlbum;

implementation

{$R *.lfm}

{ -------------------- INIT -------------------- }
procedure TpmAlbum.FormCreate(Sender: TObject);
begin
  dmMain.cDatenbank.Connected := True;

  // Open the album dataset for current user
  dmMain.qAlbum.Close;
  dmMain.qAlbum.ParamByName('UID').AsInteger := dmMain.CurrentUserID;
  dmMain.qAlbum.Open;

  // Create the model
  FAlbumModel := TAlbumModel.Create(dmMain.qAlbum);

  if Assigned(dmMain.sqAlbum) then
    dmMain.sqAlbum.OnDataChange := @AlbumDataChange;

  DisplayCurrentRecord;
end;

{ -------------------- DATA -------------------- }
function TpmAlbum.CanEditDataset: Boolean;
begin
  Result :=
    Assigned(FAlbumModel) and
    FAlbumModel.HasValidDataset;
end;

procedure TpmAlbum.AlbumDataChange(Sender: TObject; Field: TField);
begin
  DisplayCurrentRecord;
end;

procedure TpmAlbum.DisplayCurrentRecord;
var
  BlobStream: TStream;
begin
  if not CanEditDataset then
  begin
    imgAlbumCover.Picture := nil;
    Exit;
  end;

  imgAlbumCover.Picture := nil;

  if FAlbumModel.HasCover then
  begin
    BlobStream := FAlbumModel.CreateCoverStream;
    if Assigned(BlobStream) then
    try
      imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

{ -------------------- SEARCH -------------------- }
procedure TpmAlbum.edtAlbumSearchChange(Sender: TObject);
var
  FilterText: string;
begin
  FilterText := Trim(edtAlbumSearch.Text);
  dmMain.qAlbum.Filtered := False;

  if FilterText <> '' then
    dmMain.qAlbum.Filter :=
      Format('(ALBUM LIKE ''%%%s%%'') OR (ARTIST LIKE ''%%%s%%'')',
             [FilterText, FilterText]);

  dmMain.qAlbum.Filtered := FilterText <> '';
  DisplayCurrentRecord;
end;

procedure TpmAlbum.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';
  if Assigned(dmMain.qAlbum) then
    dmMain.qAlbum.Filtered := False;
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

{ -------------------- ALBUM COVER -------------------- }
procedure TpmAlbum.imgAlbumCoverClick(Sender: TObject);
begin
  if CanEditDataset then
    FAlbumModel.LoadCoverFromDialog(imgAlbumCover, dlgAlbumCover);
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

end.

