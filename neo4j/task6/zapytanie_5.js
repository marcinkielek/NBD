printjson(db.people.find(
    {"birth_date": {"$gte": ISODate("2001-01-01"), "$lte": ISODate("2100-12-31")}},
    {"_id": 0, "first_name": 1, "last_name": 1, "location.city": 1}
).toArray())
