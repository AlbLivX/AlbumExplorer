unit uAPI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, httpdefs, fpjson, jsonparser, DB, Uni, dDatenbank;

procedure OnRequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse);

implementation

procedure OnRequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  Path: string;
  Arr: TJSONArray;
  Obj: TJSONObject;
  SongID: Integer;
  IDStr: string;
begin
  AResponse.ContentType := 'application/json; charset=utf-8';
  Path := ARequest.PathInfo;

  try
    // GET /songs → list all songs
    if (ARequest.Method = 'GET') and (Path = '/songs') then
    begin
      dmMain.qSongs.Close;
      dmMain.qSongs.SQL.Text := 'SELECT ID, SongTitle, Duration FROM SONGS';
      dmMain.qSongs.Open;

      Arr := TJSONArray.Create;
      try
        while not dmMain.qSongs.EOF do
        begin
          Obj := TJSONObject.Create;
          Obj.Add('id', dmMain.qSongs.FieldByName('ID').AsInteger);
          Obj.Add('title', dmMain.qSongs.FieldByName('SongTitle').AsString);
          Obj.Add('duration', dmMain.qSongs.FieldByName('Duration').AsString);
          Arr.Add(Obj);
          dmMain.qSongs.Next;
        end;
        AResponse.Content := Arr.FormatJSON([], 2);
      finally
        Arr.Free;
      end;
    end

    // GET /songs/{id} → single song lyrics
    else if (ARequest.Method = 'GET') and (Copy(Path,1,7) = '/songs/') then
    begin
      IDStr := Copy(Path,8, Length(Path)-7);
      if not TryStrToInt(IDStr, SongID) then
      begin
        AResponse.Content := '{"error":"Invalid song ID"}';
        Exit;
      end;

      dmMain.qSongs.Close;
      dmMain.qSongs.SQL.Text := 'SELECT ID, SongTitle, Lyrics FROM SONGS WHERE ID = :id';
      dmMain.qSongs.ParamByName('id').AsInteger := SongID;
      dmMain.qSongs.Open;

      if dmMain.qSongs.EOF then
        AResponse.Content := '{"error":"Song not found"}'
      else
      begin
        Obj := TJSONObject.Create;
        try
          Obj.Add('id', dmMain.qSongs.FieldByName('ID').AsInteger);
          Obj.Add('title', dmMain.qSongs.FieldByName('SongTitle').AsString);
          Obj.Add('lyrics', dmMain.qSongs.FieldByName('Lyrics').AsString);
          AResponse.Content := Obj.FormatJSON([], 2);
        finally
          Obj.Free;
        end;
      end;
    end

    else
      AResponse.Content := '{"error":"Not found"}';

  except
    on E: Exception do
      AResponse.Content := '{"error":"' + StringReplace(E.Message, '"','\"',[rfReplaceAll]) + '"}';
  end;
end;

end.

