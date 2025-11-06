unit dDatenbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Uni, InterBaseUniProvider;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    InterBaseUniProvider1: TInterBaseUniProvider;
    cDatenbank:            TUniConnection;
    sqAdressen:            TUniDataSource;
    qAdressen:             TUniQuery;
    sqSongs:               TUniDataSource;
    qSongs:                TUniQuery;
  private

  public

  end;

  var
  dmMain: TdmMain;

implementation

{$R *.lfm}

{ TdmMain }


end.

