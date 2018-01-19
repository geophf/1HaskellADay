module Y2018.M01.D18.Solution where

{--
Do we have everything? I think we have everything.

Given an active audit record in the database, load in a set of packets from the
REST endpoint and upload the processed articles to the database.
--}

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Data.Logger
import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Solution (Packet,next,rows)
import Y2018.M01.D04.Solution (pa,storeAncilliary)
import Y2018.M01.D10.Solution (gruntWerk)
import Y2018.M01.D15.Solution (storeAuditInfo)
import Y2018.M01.D16.Solution (readPacket,endpoint)
import Y2018.M01.D17.Solution (fetchActiveAuditEntry, offset)

-- okay: read in the auditing information to get the offset, then read and
-- store n packets from that offset:

mkEntry :: Severity -> String -> LogEntry
mkEntry sev = Entry sev "etl_pilot" "Y2018.M01.D18.Solution"

loop :: Connection -> LookupTable -> LookupTable -> LookupTable
     -> Integer -> Int -> IO ()
loop _ _ _ _ offset 0 = return ()
loop conn sev activ action offset n =
   let off = show offset in
   readPacket offset >>= \pac ->
      case pac of
         Right str -> insertLogEntries conn sev 
                          [mkEntry ERROR ("Could not parse packet at offset " ++ off),
                           mkEntry ERROR ("Raw JSON for unread packet is " ++ str)]
         Left p -> gruntWerk sev pa storeAncilliary conn p           >>= \art ->
                   storeAuditInfo conn activ action p art            >>
                   putStrLn ("Processed " ++ show (p { rows = [] })) >>
                   loop conn sev activ action (fromIntegral $ next p) (pred n)

-- remember to update the offset with each iteration of the loop
-- remember to give loop the offset from the audit log to start it off.

main' :: [String] -> IO ()
main' [packs] = 
   withConnection (\conn ->
      fetchActiveAuditEntry conn                            >>= \activ ->
      lookupTable conn "severity_lk"                        >>= \sev ->
      lookupTable conn "active_lk"                          >>= \actv ->
      lookupTable conn "action_lk"                          >>= \actn ->
      loop conn sev actv actn (offset $ head activ) (read packs) >>
      putStrLn "etl process completed")
main' _ = putStrLn (unlines ["","etl_pilot n","",
   "\treads n packets from the endpoint, adjusted for offset in database", ""])
