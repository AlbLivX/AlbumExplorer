unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, dDatenbank, SongsFormUnit, DB, Uni, uConstants;

type
  { TAlbums }

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
    navAlbums:              TDBNavigator;

    procedure btnDBConnectClick     (Sender: TObject);
    procedure btnLoadAlbumCoverClick(Sender: TObject);
    procedure btnAlbumSearchClick   (Sender: TObject);
    procedure btnClearSearchClick   (Sender: TObject);
    procedure edtAlbumSearchChange  (Sender: TObject);
    procedure FormCreate            (Sender: TObject);
    procedure HandleAlbumColumnClick(Column: TColumn);

  private
    procedure DisplayCurrentRecord(DataSet: TDataSet = nil);
    procedure HandleAlbumByID     (AlbumID: Integer);
    function  CanEditDataset:     Boolean;

  public
  end;

var
  Albums: TAlbums;

implementation

{$R *.lfm}

uses
  Variants;

{===========================}
{ Form Initialization       }
{===========================}

procedure TAlbums.FormCreate(Sender: TObject);
begin
  // Configure clear search button
  if Assigned(btnClearSearch) then
  begin
    btnClearSearch.Visible     := False;
    btnClearSearch.Flat        := True;
    btnClearSearch.ShowCaption := False;
    btnClearSearch.Enabled     := True;
    btnClearSearch.Transparent := True;
    btnClearSearch.Hint        := 'Clear search';
    btnClearSearch.ShowHint    := True;
    btnClearSearch.BringToFront;
  end;

  // Assign grid column click handler
  dbgAlbums.OnCellClick := @HandleAlbumColumnClick;
end;

{===========================}
{ Dataset Utilities         }
{===========================}

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

  dbMemoAlbumDescription.Text := dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION).AsString;
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

{===========================}
{ Database Actions          }
{===========================}

procedure TAlbums.btnDBConnectClick(Sender: TObject);
begin
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
  if not Assigned(dmMain) or not Assigned(dmMain.qAdressen) then Exit;

  FilterText := Trim(edtAlbumSearch.Text);

  if FilterText = '' then
  begin
    dmMain.qAdressen.Filtered := False;
    Exit;
  end;

  dmMain.qAdressen.Filtered := False;
  dmMain.qAdressen.Filter := Format('(ALBUM LIKE ''%%%s%%'') OR (ARTIST LIKE ''%%%s%%'')',
                                    [FilterText, FilterText]);
  dmMain.qAdressen.Filtered := True;

  if not dmMain.qAdressen.IsEmpty then
    DisplayCurrentRecord;
end;

procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';
  edtAlbumSearch.SetFocus;
  if Assigned(dmMain) and Assigned(dmMain.qAdressen) then
    dmMain.qAdressen.Filtered := False;
  btnClearSearch.Visible := False;
end;

procedure TAlbums.edtAlbumSearchChange(Sender: TObject);
begin
  if Assigned(btnClearSearch) and Assigned(edtAlbumSearch) then
  begin
    btnClearSearch.Visible := edtAlbumSearch.Text <> '';
    btnClearSearch.BringToFront;
  end;
  btnAlbumSearchClick(Sender);
end;

{===========================}
{ Grid Handlers             }
{===========================}

procedure TAlbums.HandleAlbumColumnClick(Column: TColumn);
begin
  if (Column.FieldName = FIELD_ALBUM) and CanEditDataset then
    HandleAlbumByID(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
end;

procedure TAlbums.HandleAlbumByID(AlbumID: Integer);
begin
  DisplayCurrentRecord;

  case MessageDlg('Edit album (Yes) or view tracks (No)?',
                  mtConfirmation, [mbYes, mbNo], 0) of
    mrYes: dmMain.qAdressen.Edit;
    mrNo:
      begin
        if not Assigned(Tracks) then
          Application.CreateForm(TTracks, Tracks);
        Tracks.LoadSongsFromAlbum(AlbumID);
        Tracks.Show;
      end;
  end;
end;

{===========================}
{ UI Actions                }
{===========================}

procedure TAlbums.btnLoadAlbumCoverClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
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

end.

