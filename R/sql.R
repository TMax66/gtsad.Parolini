query <- "SELECT
dbo.Conferimenti.Numero As nconf,
dbo_Anag_Reparti_ConfAcc.Descrizione As repacc,
dbo.Anag_Prove.Descrizione As prova,
dbo.Anag_Specie.Descrizione, As specie,
dbo.Anag_PMS_Lomb_Comprensorio.Descrizione, As comprensorio,
dbo_Anag_P_Sesso2.Descrizione, As sex,
dbo.Anag_PMS_Lomb_Eta_Animali.Descrizione As age,
dbo.Anag_PMS_Lomb_Animale.Descrizione As animale,
dbo.Anag_PMS_Lomb_Riserva.Descrizione As riserva,
dbo.Conferimenti_PMS_Lomb.Altitudine As altitudine,
dbo_Anag_PMS_Conservazione_Materiale2.Descrizione As stat conserv,
dbo.Anag_PMS_Lomb_Segni_Malattia.Descrizione As sintomi,
dbo.Indice_Campioni_Esaminati.Numero_Campione,
dbo.Conferimenti_Campioni.Identificazione,
dbo.Nomenclatore_Range.Valore,
dbo.Nomenclatore_Range.ModEspr,
dbo.Nomenclatore_Range.ModEspr2,
dbo_Anag_Finalita_Confer.Descrizione As finalita,
dbo.Anag_Metodi_di_Prova.Descrizione As mp,
dbo.Anag_Comuni.Provincia,
dbo.Anag_Comuni.Descrizione As comune,
dbo.Conferimenti.Ind_Luogo_Prelievo As luogo, 

 {fn year(dbo.Conferimenti.Data_Prelievo)} As anno,
  {fn month(dbo.Conferimenti.Data_Prelievo)} As mese,
  dbo.Conferimenti.Data_Prelievo As dtprel
FROM
{ oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
   INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
   INNER JOIN dbo.Anag_Comuni ON ( dbo.Anag_Comuni.Codice=dbo.Conferimenti.Luogo_Prelievo )
   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
   LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
   LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova ON ( dbo.Nomenclatore_MP.MP=dbo.Anag_Metodi_di_Prova.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
   LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
   INNER JOIN dbo.Indice_Campioni_Esaminati ON ( dbo.Esami_Aggregati.Anno_Conferimento=dbo.Indice_Campioni_Esaminati.Anno_Conferimento and dbo.Esami_Aggregati.Numero_Conferimento=dbo.Indice_Campioni_Esaminati.Numero_Conferimento and dbo.Esami_Aggregati.Codice=dbo.Indice_Campioni_Esaminati.Codice )
   LEFT OUTER JOIN dbo.Risultati_Analisi ON ( dbo.Indice_Campioni_Esaminati.Anno_Conferimento=dbo.Risultati_Analisi.Anno_Conferimento and dbo.Indice_Campioni_Esaminati.Numero_Conferimento=dbo.Risultati_Analisi.Numero_Conferimento and dbo.Indice_Campioni_Esaminati.Codice=dbo.Risultati_Analisi.Codice and dbo.Indice_Campioni_Esaminati.Numero_Campione=dbo.Risultati_Analisi.Numero_Campione )
   LEFT OUTER JOIN dbo.Nomenclatore_Range ON ( dbo.Risultati_Analisi.Range=dbo.Nomenclatore_Range.Codice )
   LEFT OUTER JOIN dbo.Conferimenti_Campioni ON ( dbo.Conferimenti_Campioni.Anno=dbo.Indice_Campioni_Esaminati.Anno_Conferimento and dbo.Conferimenti_Campioni.Numero=dbo.Indice_Campioni_Esaminati.Numero_Conferimento and dbo.Conferimenti_Campioni.Campione=dbo.Indice_Campioni_Esaminati.Numero_Campione )
   LEFT OUTER JOIN dbo.Anag_Specie ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
   INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
   INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
   LEFT OUTER JOIN dbo.Conferimenti_PMS_Lomb ON ( dbo.Conferimenti_PMS_Lomb.Anno_Conferimento=dbo.Conferimenti.Anno and dbo.Conferimenti_PMS_Lomb.Numero_Conferimento=dbo.Conferimenti.Numero )
   LEFT OUTER JOIN dbo.Anag_PMS_Conservazione_Materiale  dbo_Anag_PMS_Conservazione_Materiale2 ON ( dbo.Conferimenti_PMS_Lomb.Stato_Conservazione=dbo_Anag_PMS_Conservazione_Materiale2.Codice )
   LEFT OUTER JOIN dbo.Anag_PMS_Lomb_Animale ON ( dbo.Conferimenti_PMS_Lomb.Animale=dbo.Anag_PMS_Lomb_Animale.Codice )
   LEFT OUTER JOIN dbo.Anag_PMS_Lomb_Comprensorio ON ( dbo.Conferimenti_PMS_Lomb.Comprensorio=dbo.Anag_PMS_Lomb_Comprensorio.Codice )
   LEFT OUTER JOIN dbo.Anag_PMS_Lomb_Eta_Animali ON ( dbo.Conferimenti_PMS_Lomb."Età"=dbo.Anag_PMS_Lomb_Eta_Animali.Codice )
   LEFT OUTER JOIN dbo.Anag_PMS_Lomb_Riserva ON ( dbo.Conferimenti_PMS_Lomb.Riserva=dbo.Anag_PMS_Lomb_Riserva.Codice )
   LEFT OUTER JOIN dbo.Anag_PMS_Lomb_Segni_Malattia ON ( dbo.Conferimenti_PMS_Lomb.Segni_Malattia=dbo.Anag_PMS_Lomb_Segni_Malattia.Codice )
   LEFT OUTER JOIN dbo.Anag_P_Sesso  dbo_Anag_P_Sesso2 ON ( dbo.Conferimenti_PMS_Lomb.Sesso=dbo_Anag_P_Sesso2.Codice )
  }
WHERE
  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  (
  {fn year(dbo.Conferimenti.Data_Prelievo)}  >=  2017
  AND  dbo.Anag_Metodi_di_Prova.Descrizione  IN  ('MP 01/163 rev. 0', 'MP 01/163 rev. 1', 'MP 01/163 rev. 2', 'MP 04/125 rev. 1')
  AND  dbo_Anag_Finalita_Confer.Descrizione  =  'Monitoraggio fauna selvatica Lombardia'
  )
"
