unit LyricsFetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, OpenSSLSockets;

type
  TLyricsFetcher = class
  public
    class function GetLyricsFromAPI(SongID: Integer; const Title, Artist: string): string; static;
  end;

implementation

class function TLyricsFetcher.GetLyricsFromAPI(SongID: Integer; const Title, Artist: string): string;
var
  Client: TFPHTTPClient;
  Response: string;
  JSON: TJSONData;
  LyricsField: TJSONData;
  URL, EncArtist, EncTitle: string;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  try
    // Encode artist/title for URL
    EncArtist := StringReplace(Artist, ' ', '%20', [rfReplaceAll]);
    EncTitle  := StringReplace(Title,  ' ', '%20', [rfReplaceAll]);

    URL := Format('https://api.lyrics.ovh/v1/%s/%s', [EncArtist, EncTitle]);

    try
      Response := Client.Get(URL);

      JSON := GetJSON(Response);
      try
        LyricsField := JSON.FindPath('lyrics');
        if Assigned(LyricsField) then
          Result := Trim(LyricsField.AsString);
      finally
        JSON.Free;
      end;
    except
      on E: Exception do
        writeln(' Error fetching lyrics for "', Title, '": ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

end.

