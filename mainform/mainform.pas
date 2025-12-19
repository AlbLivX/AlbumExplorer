unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, Buttons, Menus, DB,
  dDatenbank, SongsFormUnit, DBCtrls, Grids, ExtDlgs, Types;

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

    procedure dbgAlbumsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure edtAlbumSearchChange(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnClearSearchVisible(Sender: TObject);
    procedure edtAlbumSearchExit(Sender: TObject);
    procedure miEditAlbumClick(Sender: TObject);
    procedure miViewTracksClick(Sender: TObject);
    procedure imgAlbumCoverClick(Sender: TObject);
    procedure dbgAlbumsCellClick(Column: TColumn);

  private
    procedure DisplayCurrentRecord;
    procedure AlbumDataChange(Sender: TObject; Field: TField);
    function CanEditDataset: Boolean;
  end;

var
  pmAlbum: TpmAlbum;

implementation

{$R *.lfm}

{ -------------------- INIT -------------------- }

procedure TpmAlbum.FormCreate(Sender: TObject);
begin
  dmMain.cDatenbank.Connected := True;
  dmMain.qAlbum.Open;

  // Hook DataSource change to automatically update cover
  if Assigned(dmMain.qAlbum.DataSource) then
    dmMain.qAlbum.DataSource.OnDataChange := @AlbumDataChange;

  DisplayCurrentRecord;
end;

procedure TpmAlbum.dbgAlbumsMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  DisplayCurrentRecord;
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

procedure TpmAlbum.DisplayCurrentRecord;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then
  begin
    imgAlbumCover.Picture := nil;
    Exit;
  end;

  imgAlbumCover.Picture := nil;

  Field := dmMain.qAlbum.FindField('ALBUMCOVER');
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qAlbum.CreateBlobStream(Field, bmRead);
    try
      if BlobStream.Size > 0 then
        imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

{ -------------------- SEARCH -------------------- }

procedure TpmAlbum.edtAlbumSearchChange(Sender: TObject);
begin
  dmMain.qAlbum.Close;
  dmMain.qAlbum.ParamByName('UID').AsInteger := dmMain.CurrentUserID;
  dmMain.qAlbum.ParamByName('SEARCH').AsString := '%' + Trim(edtAlbumSearch.Text) + '%';
  dmMain.qAlbum.Open;

  DisplayCurrentRecord;
end;




procedure TpmAlbum.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';

  dmMain.qAlbum.Close;
  dmMain.qAlbum.ParamByName('UID').AsInteger := dmMain.CurrentUserID;
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

procedure TpmAlbum.imgAlbumCoverClick(Sender: TObject);
var
  Field: TField;
begin
  if not CanEditDataset then Exit;

  if dlgAlbumCover.Execute then
  begin
    dmMain.qAlbum.Edit;
    Field := dmMain.qAlbum.FindField('ALBUMCOVER');
    if Field is TBlobField then
      TBlobField(Field).LoadFromFile(dlgAlbumCover.FileName);
    dmMain.qAlbum.Post;
  end;

  DisplayCurrentRecord;
end;

end.

