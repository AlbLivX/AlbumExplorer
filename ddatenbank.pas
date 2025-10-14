unit dDatenbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Uni, InterBaseUniProvider;

type
  { TdmMain }

  TdmMain = class(TDataModule)
    InterBaseUniProvider1: TInterBaseUniProvider;
    cDatenbank: TUniConnection;
    qAdressen: TUniQuery;
    sqAdressen: TUniDataSource;
    qSongs: TUniQuery;
    sqSongs: TUniDataSource;
  private
  public
  end;

var
  dmMain: TdmMain;


implementation

{$R *.lfm}

end.
