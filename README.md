# RE6013

**Variable Description**

1. **tracking_id**: Tracking ID of each observation
2. **datetime**: Datetime of observation taken; later split into the year, month, day of the month, day of the week, hour and minutes
3. **wind_speed(m/s)**: Speed of the wind
4. **atmospheric_temperature(°C)**: Atmospheric temperature (assume not to be at ground level as there is another variable for land temperature)
5. **shaft_temperature(°C)**: The shaft connects the turbine to the generator, turning at the same speed as the turbine. 
6. **blades_angle(°)**: Wind turbine blades have to be streamlined so they can efficiently pass through the air. Changing the angle of the blades will change the area facing the apparent wind. This is why blade pitch angles of 10- 20 degrees tend to have much less drag than greater angles.
7. **gearbox_temperature(°C)**: A gearbox is often used in a wind turbine to increase the rotational speed from a low-speed main shaft to a high-speed shaft connecting with an electrical generator. It is connected to the shaft and the generator. 
8. **engine_temperature(°C)**: An engine is also a motor, it is required to convert electrical energy to mechanical energy (likely for the turbine’s own usage) 
9. **motor_torque(N-m)**: Most wind turbines extract power from the wind in mechanical form and transmit it to the load by rotating shafts. When power is being transmitted through a shaft, a torque T will be present. 
10. **generator_temperature(°C)**: Temperature of the generator
11. **atmospheric_pressure(Pascal)**: Atmospheric pressure (assume to be at sea level)
12. **area_temperature(°C)**: Assumed to be the temperatures of the surroundings in which the wind turbine is being built
13. **windmill_body_temperature(°C)**: As the name describes
14. **wind_direction(°)**: As the name describes
15. **resistance(ohm)**: Likely refer to the load resistance (cumulative resistance of a circuit in the windmill)
16. **rotor_torque(N-m)**: The rotor torque is the torque the electrical motor develops when its starts at rest or zero speed. The magnitude of the rotor current is determined by motor horsepower and design characteristics
17. **turbine_status**: Categorical variable with categories:  'A''A2''AAA''AB''ABC''AC''B''B2''BA''BB''BBB''BCB''BD''D'(no further explanation can be found on what these categories represent) 
18. **cloud_level**: Categorical variable with categories:  'Extremely Low''Low''Medium'
19. **blade_length(m)**: As the name describes
20. **blade_breadth(m)**: As the name describes
21. **windmill_height(m)**: As the name describes
22. **windmill_generated_power(kW/h)** As the name describes

**Note**
1. There should be some strong correlation between gearbox_temperature, shaft_temparature, generator_temperature and engine_temperature
2. Motor-torque is like to be positively correlated with generated power
