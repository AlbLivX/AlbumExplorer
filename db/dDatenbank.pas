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
    qSongsALBUMCOVER:      TBlobField;
    qSongsARTIST:          TStringField;
    qSongsID:              TLongintField;
    qSongsLYRICS:          TStringField;
    qSongsSONGTITLE:       TStringField;
    sqAdressen:            TUniDataSource;
    qAdressen:             TUniQuery;
    sqSongs:               TUniDataSource;
    qSongs:                TUniQuery;
    sqUsers: TUniDataSource;
    qUsers: TUniQuery;
  private

  public

  end;

var
  dmMain: TdmMain;

implementation

{$R *.lfm}

{ TdmMain }




end.

