unit dDatenbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Uni, DB, InterBaseUniProvider;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    InterBaseUniProvider1: TInterBaseUniProvider;
    cDatenbank:            TUniConnection;
    sqAdressen:            TUniDataSource;
    qAdressen:             TUniQuery;
    sqSongs:               TUniDataSource;
    qSongs:                TUniQuery;
    sqUsers:               TUniDataSource;
    qUsers:                TUniQuery;


  private

  public

  end;



implementation

{$R *.lfm}

{ TdmMain }






end.

