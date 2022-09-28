CREATE POLICY thing ON messages m1
    FOR INSERT WITH CHECK
        (EXISTS (
            SELECT * FROM
                users u1
            WHERE
                u1.id = current_user
            AND
                u1.alcohol_ppm < 5
            ))
