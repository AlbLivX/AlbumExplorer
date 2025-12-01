unit dDatenbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Uni, InterBaseUniProvider, DB;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    cDatenbank:            TUniConnection;
    InterBaseUniProvider1: TInterBaseUniProvider;
    qNextUserID: TUniQuery;
    qSongsALBUMCOVER:      TBlobField;
    qSongsARTIST:          TStringField;
    qSongsID:              TLongintField;
    qSongsLYRICS: TStringField;
    //qSongsLYRICS:          TStringField;
    qSongsSONGTITLE:       TStringField;
    sqAdressen:            TUniDataSource;
    qAdressen:             TUniQuery;
    sqSongs:               TUniDataSource;
    qSongs:                TUniQuery;
    sqUsers:               TUniDataSource;
    qUsersLogin:           TUniQuery;
    qUsersRegister:        TUniQuery;
    sqUsersRegister:       TUniDataSource;



  private

  public

  end;

var
  dmMain: TdmMain;

implementation

{$R *.lfm}

{ TdmMain }



end.

