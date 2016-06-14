/* Extract plankton counts data from BioChem database */
/* Data retrieval based on custom sample_id's associated with AZMP transects for Maritimes region */
SELECT
   ev.collector_event_id || '_' || ph.collector_sample_id custom_sample_id,
   pg.split_fraction,
   pg.min_sieve,
   pg.max_sieve,
   ntc.best_NODC7,
   ntc.TSN,
   ntc.taxonomic_name,
   lh.name stage,
   lh.molt_number,
   sx.name sex,
   pg.counts
FROM
   biochem.bcevents ev,
   biochem.bclifehistories lh,
   biochem.bcnatnltaxoncodes ntc,
   biochem.bcplanktngenerals pg,
   biochem.bcplanktnhedrs ph,
   biochem.bcsexes sx
WHERE
   /* link tables */
   ev.event_seq = ph.event_seq
   AND lh.life_history_seq = pg.life_history_seq
   AND ntc.national_taxonomic_seq = pg.national_taxonomic_seq
   AND ph.plankton_seq = pg.plankton_seq
   AND sx.sex_seq = pg.sex_seq
   /* non null data only */
   AND pg.counts IS NOT NULL
   /* custom sample_id filter */
   AND (ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_1999 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2000 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2001 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2002 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2003 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2004 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2005 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2006 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2007 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2008 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2009 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2010 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2011 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2012 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2013 tmp WHERE tmp.FLAG=1)
       OR ev.collector_event_id || '_' || ph.collector_sample_id IN (SELECT tmp.custom_sample_id FROM MAR_PL_2014 tmp WHERE tmp.FLAG=1)
       )
ORDER BY
   ph.start_date,
   ph.start_time,
   ntc.best_NODC7