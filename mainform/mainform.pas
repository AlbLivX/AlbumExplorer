unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, dDatenbank, SongsFormUnit, DB, Uni;

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



    procedure btnAlbumSearchClick    (Sender: TObject);
    procedure btnClearSearchClick   (Sender: TObject);
    procedure btnDBConnectClick      (Sender: TObject);
    procedure btnLoadAlbumCoverClick (Sender: TObject);
    procedure colCellClick           (Column: TColumn);
    procedure edtAlbumSearchChange   (Sender: TObject);

  private
    procedure DisplayCurrentRecord (DataSet: TDataSet = nil);
    procedure HandleAlbumClick     (AlbumID: Integer);
    function  CanEditDataset:      Boolean;

  public

  end;

const
  FIELD_ALBUM_COVER = 'AlbumCover';
  FIELD_DESCRIPTION = 'Description';
  FIELD_ID = 'ID';
  FIELD_ALBUM = 'ALBUM';

var
  Albums:     TAlbums;

implementation

{$R *.lfm}

//dataset available?
function TAlbums.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
         and dmMain.qAdressen.Active
         and not dmMain.qAdressen.IsEmpty;
end;

//Description and Album Cover
procedure TAlbums.DisplayCurrentRecord(DataSet: TDataSet);
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  dbMemoAlbumDescription.Text := dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION).AsString;
  imgAlbumCover.Picture := nil;

  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  if not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

//Connects to db and loads albums ds
procedure TAlbums.btnDBConnectClick(Sender: TObject);
begin
  writeln('Connect button clicked!');

  if not Assigned(dmMain) then
  begin
    writeln('dmMain is not assigned!');
    Exit;
  end;

  try
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAdressen.Active then
      dmMain.qAdressen.Open;

    writeln('Dataset opened. Record count: ', dmMain.qAdressen.RecordCount);

    DisplayCurrentRecord;

    dmMain.qAdressen.AfterScroll := @DisplayCurrentRecord;
    dbgAlbums.OnCellClick := @colCellClick;

  except
    on E: Exception do
      writeln('Database error: ', E.Message);
  end;
end;

procedure TAlbums.btnAlbumSearchClick(Sender: TObject);
var
  FilterText: string;
begin
  if not Assigned(dmMain) or not dmMain.qAdressen.Active then Exit;

  FilterText := Trim(edtAlbumSearch.Text);
  //Clear filter if empty
  if FilterText = '' then
  begin
    dmMain.qAdressen.Filtered := False;
    Exit;
  end;

  //Apply Filter
  dmMain.qAdressen.Filtered   := False;
  dmMain.qAdressen.Filter := Format('(ALBUM LIKE ''%%%s%%'') OR (ARTIST LIKE ''%%%s%%'')',
                                   [FilterText, FilterText]);
  dmMain.qAdressen.Filtered   := True;

  //Automatically display the first record’s cover after filtering
  if not dmMain.qAdressen.ISEmpty then
    DisplayCurrentRecord;
end;


procedure TAlbums.btnClearSearchClick(Sender: TObject);
begin
  edtAlbumSearch.Text := '';
  edtAlbumSearch.SetFocus;
  dmMain.qAdressen.Filtered := False;
    btnAlbumSearchClick(Sender);
end;


 procedure TAlbums.edtAlbumSearchChange(Sender: TObject);
begin
  if Assigned(btnClearSearch) and Assigned(edtAlbumSearch) then
  begin
    btnClearSearch.Visible := edtAlbumSearch.Text <> '';
    btnAlbumSearchClick(Sender);
  end;
end;

//Called on cell click in the table
procedure TAlbums.colCellClick(Column: TColumn);
begin
  if (Column.FieldName = FIELD_ALBUM) and CanEditDataset then
    HandleAlbumClick(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
end;

//on an Album Click Handler
procedure TAlbums.HandleAlbumClick(AlbumID: Integer);
begin
  DisplayCurrentRecord;

  case MessageDlg('Edit album (Yes) or view tracks (No)?',
                  mtConfirmation, [mbYes, mbNo], 0) of
    mrYes:
      begin
        dmMain.qAdressen.Edit;
      end;
    mrNo:
      begin
        if not Assigned(Tracks) then
          Application.CreateForm(TTracks, Tracks);

        Tracks.LoadSongsFromAlbum(AlbumID);
        Tracks.Show;
      end;
  end;
end;

//loads +/ saves imgs  to Album Record
procedure TAlbums.btnLoadAlbumCoverClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  if not CanEditDataset then Exit;
  if not dlgAlbumCover.Execute then Exit;

  try
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
  except
    on E: Exception do
      ShowMessage('Error saving image: ' + E.Message);
  end;
end;

end.

