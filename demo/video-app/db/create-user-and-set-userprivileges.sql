CREATE USER IF NOT EXISTS video_user_dml@localhost;

GRANT SELECT,
	INSERT,
	UPDATE,
	DELETE
ON	video.*
TO	video_user_dml@localhost;
