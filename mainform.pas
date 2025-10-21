unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, RTTICtrls, dDatenbank, SongsFormUnit, DB,
  Uni;

type
  { TAlbums }

  TAlbums = class(TForm)
    btnConnect: TButton;
    btnLoadImage: TButton;
    col: TDBGrid;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    Img: TImage;
    OpenDialog1: TOpenDialog;
    procedure btnConnectClick(Sender: TObject);
    procedure btnConnectTestClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure colCellClick(Column: TColumn);
  private
    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
    procedure qAdressenAfterScroll(DataSet: TDataSet);
    procedure HandleAlbumClick(AlbumID: Integer);
    procedure Log(const Msg: string);
  public
  end;

const
  FIELD_ALBUM_COVER = 'AlbumCover';
  FIELD_DESCRIPTION = 'Description';
  FIELD_ID = 'ID';
  FIELD_ALBUM = 'ALBUM';

var
  Albums: TAlbums;

implementation

{$R *.lfm}

uses
  Variants;

procedure TAlbums.Log(const Msg: string);
var
  LogFile: TextFile;
  LogPath: string;
begin
  try
    LogPath := ExtractFilePath(ParamStr(0)) + 'logs.txt';
    AssignFile(LogFile, LogPath);
    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
    CloseFile(LogFile);
  except
    // Logging should never raise errors
  end;
end;

procedure TAlbums.DisplayCurrentRecord;
var
  Field: TField;
  BlobStream: TStream;
begin
  //ShowMessage('Displaying current album record...');

  if not CanEditDataset then
  begin
    //ShowMessage('Cannot display record: Dataset not editable or empty.');
    Exit;
  end;

  Field := dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION);
  if Assigned(Field) and not Field.IsNull then
    DBMemo1.Text := Field.AsString
  else
    DBMemo1.Clear;

  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  Img.Picture := nil;

  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      Img.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;

  //ShowMessage('Album description and image displayed.');
end;

function TAlbums.CanEditDataset: Boolean;
begin
  Result := (Assigned(dmMain)) and (dmMain.qAdressen.Active) and (not dmMain.qAdressen.IsEmpty);
  if Result then
    //ShowMessage('Dataset is ready for editing.')
  else
    //ShowMessage('Dataset is not ready (inactive or empty).');
end;

procedure TAlbums.qAdressenAfterScroll(DataSet: TDataSet);
begin
  //ShowMessage('Scrolled to a new album record.');
  DisplayCurrentRecord;
end;

procedure TAlbums.btnConnectClick(Sender: TObject);
begin
  //ShowMessage('Connect button clicked.');

  if not Assigned(dmMain) then
  begin
    //ShowMessage('Data module not assigned.');
    Exit;
  end;

  try
    if not dmMain.cDatenbank.Connected then
    begin
      dmMain.cDatenbank.Connected := True;
      Log('Database connected.');
      //ShowMessage('Connected to database.');
    end;
  except
    on E: EDatabaseError do
    begin
      Log('Database connection failed: ' + E.Message);
      //ShowMessage('Database connection failed: ' + E.Message);
      Exit;
    end;
  end;

  try
    if not dmMain.qAdressen.Active then
    begin
      dmMain.qAdressen.Open;
      Log('Albums dataset opened.');
      //ShowMessage('Albums dataset opened.');
    end;
  except
    on E: EDatabaseError do
    begin
      Log('Failed to open albums dataset: ' + E.Message);
      //ShowMessage('Failed to open albums dataset: ' + E.Message);
      Exit;
    end;
  end;

  col.OnCellClick := @colCellClick;
  dmMain.qAdressen.AfterScroll := @qAdressenAfterScroll;
  //ShowMessage('Events wired: Cell click and AfterScroll.');
end;

procedure TAlbums.btnConnectTestClick(Sender: TObject);
begin
  //ShowMessage('Test button clicked.');
end;

procedure TAlbums.colCellClick(Column: TColumn);
begin
  //ShowMessage('Column cell clicked: ' + Column.FieldName);

  if (Column.FieldName = FIELD_ALBUM) and CanEditDataset then
  begin
    //ShowMessage('You clicked on ALBUM column, loading album...');
    HandleAlbumClick(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
  end;
end;

procedure TAlbums.HandleAlbumClick(AlbumID: Integer);
var
  Choice: Integer;
begin
  //ShowMessage('Handling click on album with ID = ' + IntToStr(AlbumID));

  DisplayCurrentRecord;

  Choice := MessageDlg('For editing album click "Yes", for viewing tracks click "No".',
                       mtConfirmation, [mbYes, mbNo], 0);

  if Choice = mrYes then
  begin
    if not (dmMain.qAdressen.State in [dsEdit, dsInsert]) then
      dmMain.qAdressen.Edit;

    Log('Editing album with ID ' + IntToStr(AlbumID));
    //ShowMessage('Album is now in edit mode.');
  end
  else
  begin
    if not Assigned(Tracks) then
      Application.CreateForm(TTracks, Tracks);

    Tracks.LoadSongsFromAlbum(AlbumID);
    Tracks.Show;
    Log('Viewing tracks for album ID ' + IntToStr(AlbumID));
    //ShowMessage('Track list opened for album ID ' + IntToStr(AlbumID));
  end;
end;

procedure TAlbums.btnLoadImageClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  //ShowMessage('Load Image button clicked.');

  if not CanEditDataset then
  begin
    //ShowMessage('Cannot load image: Dataset not ready.');
    Exit;
  end;

  if OpenDialog1.Execute then
  begin
    try
      Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
      if not Assigned(Field) or (Field.DataType <> ftBlob) then
        raise Exception.Create('AlbumCover field missing or not a BLOB');

      if not (dmMain.qAdressen.State in [dsEdit, dsInsert]) then
        dmMain.qAdressen.Edit;

      FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
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
      Log('Image loaded and saved for current album.');
      //ShowMessage('Image saved successfully!');
    except
      on E: EStreamError do
      begin
        Log('Stream error while saving image: ' + E.Message);
        //ShowMessage('Stream error: ' + E.Message);
      end;
      on E: EDatabaseError do
      begin
        Log('Database error while saving image: ' + E.Message);
        //ShowMessage('Database error: ' + E.Message);
      end;
      on E: Exception do
      begin
        Log('Unknown error while saving image: ' + E.Message);
        //ShowMessage('Error saving image: ' + E.Message);
      end;
    end;
  end
  else
  begin
    //ShowMessage('Image selection cancelled.');
  end;
end;

end.

