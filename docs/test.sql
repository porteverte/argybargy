DROP TABLE IF EXISTS TESTDB.dbo.tbl_test_copy;
SELECT TOP 50 * INTO TESTDB.dbo.tbl_test_copy FROM TESTDB.dbo.tbl_test;
--SELECT * FROM TESTDB.dbo.tbl_test_copy

--SELECT * FROM TESTDB.dbo.tbl_new