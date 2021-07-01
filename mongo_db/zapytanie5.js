db.people.find(
  {
    birth_date: {
      $gte: ISODate('2001-01-01T00:00:00Z'),
      $lt: ISODate('2100-12-31T00:00:00Z')
    }
  },
  { first_name: 1, last_name: 1, location: { city: 1 } }
);
