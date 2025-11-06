unit SearchHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Uni, DB;

implementation

procedure SearchAlbums(SearchText: string; var Query: TUniQuery; var DataSetToLocate: TUniQuery);
begin
  if not Assigned(Query) then Exit;

  Query.Close;
  Query.ParamByName('search').AsString := '%' + Trim(SearchText) + '%';
  Query.Open;

  if not Query.IsEmpty then
    DataSetToLocate.Locate('ID', Query.FieldByName('ID').AsInteger, []);
end;

end.


