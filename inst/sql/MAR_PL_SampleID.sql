/* Extract plankton sample_id from BioChem database */
/* Data retrieval based on mission_id associated with AZMP transects for Maritimes region */
SELECT DISTINCT
   mi.descriptor mission_descriptor,
   ev.collector_event_id,
   ph.collector_sample_id,
   ev.collector_event_id || '_' || ph.collector_sample_id custom_sample_id,
   ev.collector_station_name,
   ph.start_date,
   ph.start_time,
   ph.start_lat,
   ph.start_lon,
   ph.start_depth,
   ph.end_depth,
   ph.sounding,
   ph.volume,
   ge.model gear_model,
   ph.mesh_size,
   ph.meters_sqd_flag,
   mi.name mission_name,
   ph.collector_deployment_id,
   ph.collector collector_name,
   ph.collector_comment,
   dc.name data_center
FROM
   biochem.bcdatacenters dc,
   biochem.bcevents ev,
   biochem.bcgears ge,
   biochem.bcmissions mi,
   biochem.bcplanktngenerals pg,
   biochem.bcplanktnhedrs ph
WHERE
   /* link tables */
   ge.gear_seq = ph.gear_seq
   AND ev.event_seq = ph.event_seq
   AND dc.data_center_code = ph.data_center_code
   AND mi.mission_seq = ev.mission_seq
   AND ph.plankton_seq = pg.plankton_seq
   /* date filter */
   AND TO_CHAR(ph.start_date, 'YYYY/MM/DD') >= '1999/01/01'
   AND TO_CHAR(ph.start_date, 'YYYY/MM/DD') <= '2014/12/31'
   /* gear filter */
   AND ge.model IN ('Ring net 0.75m','Ring net 0.75m xbow')
   AND ph.mesh_size BETWEEN 199 AND 203
   /* data filter */
   AND ph.meters_sqd_flag = 'Y'
   /* spring mission_id filter */
   AND (mi.descriptor IN ('18HU99005','18PZ00002','18HU01009','2002916','18HU03005','18HU04009','18NE05004','18HU06008','18HU07001','18HU08004','18HU09005','18HU10006','18HU11004','18HU13004','18HU14004')
   /* 2012 HL winter groundfish mission_id/event_id filter */
   OR (mi.descriptor = '18NE12002' AND ev.collector_event_id IN ('2012002143','2012002144','2012002145','2012002146','2012002147','2012002148','2012002149'))
   /* fall mission_id filter */
   OR mi.descriptor IN ('18HU99054','18HU00050','18HU01061','18HU02064','18HU03067','18HU04055','18HU05055','18HU06052','18HU07045','18HU08037','18HU09048','18HU10070','18HU11043','18HU12042','18HU13037','18HU14030')
   )
   /* non null data only */
   AND (pg.counts IS NOT NULL OR pg.wet_weight IS NOT NULL OR pg.dry_weight IS NOT NULL)
ORDER BY
   ph.start_date,
   ph.start_time
   