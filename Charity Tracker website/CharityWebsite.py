from flask import Flask, request, jsonify
from flask_sqlalchemy import SQLAlchemy

# Initialize Flask app and database connection
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///charities.db'
db = SQLAlchemy(app)

# Define database schema
class Charity(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(50), unique=True, nullable=False)
    total_donations = db.Column(db.Float, nullable=False, default=0.0)

    def __repr__(self):
        return f"Charity('{self.name}', '{self.total_donations}')"

# Define API endpoints
@app.route('/donation', methods=['POST'])
def add_donation():
    # Get the donation details from the request body
    donation = request.get_json()
    charity_name = donation['charity']
    amount = donation['amount']

    # Check if the charity exists
    charity = Charity.query.filter_by(name=charity_name).first()

    if charity:
        # If the charity exists, add the amount to the total donations
        charity.total_donations += amount
        db.session.commit()
    else:
        # If the charity doesn't exist, create a new charity and add the amount
        charity = Charity(name=charity_name, total_donations=amount)
        db.session.add(charity)
        db.session.commit()

    return jsonify({'message': 'Donation added successfully'})

@app.route('/charities', methods=['GET'])
def get_charities():
    # Get the list of all charities and their total donations
    charities = Charity.query.all()

    # Format the data as a dictionary with charity names as keys and total donations as values
    data = {charity.name: charity.total_donations for charity in charities}

    return jsonify(data)

# Start the app
if __name__ == '__main__':
    app.run(debug=True)